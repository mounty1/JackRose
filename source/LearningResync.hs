{-|
Description: Ensure learning data are up to date with external data sources.
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Data sources may be updated externally to JR.
-}


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}


module LearningResync (update) where


import Data.Time (getCurrentTime, UTCTime)
import Control.Exception (catch)
import Data.List (foldl', intersperse)
import Data.List.Ordered (minus)
import qualified Database.HDBC as HDBC (SqlError, SqlColDesc, describeTable, prepare, sExecute, sFetchAllRows)
import qualified Data.Map as DM (insert)
import qualified Data.Text as DT (Text, concat, pack, unpack, null)
import qualified JRState (JRState(..), runFilteredLoggingT)
import Database.Persist.Sql (Entity(Entity), Key, fromSqlKey)
import Database.Persist.Sqlite (runSqlPool, selectList)
import Database.Persist (replace)
import Database.Persist.Sql (insertBy)
import TextList (enSerialise)
import Data.Maybe (catMaybes)
import LearningData (DataSource(..), DataRow(..), DataSourceId, LearnDatum(..), mkLearnDatum, ViewId, deleteItems, ascendingSourceKeys, viewsOnDataSource)
import Database.HDBC.Statement (Statement)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logWarnNS, logErrorNS)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import ConnectionSpec (DataDescriptor(..), DataHandle(..))
import MaybeIntValue (maybeIntValue)
import TextList (deSerialise)
import DeckData (userDeckEndsViewed, userDeckEndViewId, UserDeckEnd)
import Authorisation (UserId, userList)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (SqlBackend)
import TextShow (showt)


-- | Create handles or connections to all data sources;
-- | then read them to be sure all records are known.
update :: JRState.JRState -> IO ()
update site = JRState.runFilteredLoggingT site $ runSqlPool (selectList [] []) (JRState.tablesFile site)
			-- convert the list of data-source rcords into a map of open connections
			>>= foldl' (updateOneSource site) (liftIO $ return [])
			-- then merge that make into the held set
			-- TODO: what about when the key is already in the map?
			-- The code above returns a list of (key, descriptor) pairs
			-- The code below folds them into the TVar set, which in the current implementation
			-- will be empty because this code is being evaluated at start-up time.
			>>= liftIO . atomically . modifyTVar' (JRState.dataSchemes site) . (flip $ foldr $ uncurry DM.insert)


type DataSchemes = LoggingT IO [(DataSourceId, DataDescriptor)]


type RowResult a = ReaderT SqlBackend (LoggingT IO) [a]


updateOneSource :: JRState.JRState -> DataSchemes -> Entity DataSource -> DataSchemes

updateOneSource site schemeMap (Entity dataSourceId dataSourceParts) = liftIO (connection site (deSerialise dataSourceString)) >>= either badConnString reSyncRows where

	badConnString word = logWarnNS dataNameString (DT.concat [dataSourceString, ": error: ", word]) >> schemeMap

	reSyncRows :: OpenDataSource -> DataSchemes
	reSyncRows (OpenDataSource openConn colheads _ dataKeysList) = liftIO getCurrentTime
		>>= updateSync dataKeysList
		>> schemeMap
		>>= liftIO . return . (:) (dataSourceId, DataDescriptor colheads dataSourceId openConn)

	updateSync dataKeysList timeStamp = runSqlPool
			(ascendingSourceKeys dataSourceId >>= updateSource timeStamp . partitionInsertResults dataKeysList)
			(JRState.tablesFile site)

	-- Update time-stamp only if new data_rows were inserted;  result is new rows inserted.
	updateSource :: UTCTime -> ([DT.Text], [DT.Text]) -> RowResult (Key LearnDatum)
	updateSource _ ([], []) = return []
	updateSource timeStamp (deleted, newItems) = replace dataSourceId dataSourceParts{dataSourceResynced = timeStamp}
		>> deleteItems deleted
		>> viewsOnDataSource dataSourceId
		>>= expandViewList
		>>= \userViewPairs -> (fmap concat $ sequence $ map (insertDataRow userViewPairs timeStamp) newItems)

	-- insert new data row, then if successful, new learn datum for all valid combinations of (user, view)
	insertDataRow :: [(UserId, ViewId)] -> UTCTime -> DT.Text -> RowResult (Key LearnDatum)
	insertDataRow userViewPairs timeStamp itemKey = insertBy (DataRow itemKey dataSourceId timeStamp) >>= either alreadyDataRowHuh (insertLearnData userViewPairs timeStamp)

	-- this was supposed to be a new row, but its record already exists.
	alreadyDataRowHuh (Entity _ (DataRow k _ _)) = alreadyRowPresent "data row" k

	-- data row inserted, so insert new learn data.
	insertLearnData :: [(UserId, ViewId)] -> UTCTime -> Key DataRow -> RowResult (Key LearnDatum)
	insertLearnData userViewPairs timeStamp rowId = fmap concat $ sequence $ map (insertLearnDatum timeStamp rowId) userViewPairs

	-- insert one learn datum;  (return . return) makes a RowResult from a LearnDatum
	insertLearnDatum :: UTCTime -> Key DataRow -> (UserId, ViewId) -> RowResult (Key LearnDatum)
	insertLearnDatum timeStamp itemId (user, view) = insertBy(mkLearnDatum view itemId user 0 timeStamp) >>= either alreadyDatumHuh (return . return)

	-- as above, this should not happen.
	alreadyDatumHuh (Entity _ (LearnDatum vId _ _ _ _)) = alreadyRowPresent "learn datum" (showt $ fromSqlKey vId)

	alreadyRowPresent label rowId = logErrorNS dataNameString (DT.concat [dataSourceString, ": ", label, " \"", rowId, " already exists"]) >> return []

	dataSourceString = dataSourceSourceSerial dataSourceParts
	dataNameString = dataSourceName dataSourceParts


-- this is where we would call isect to get the common (unchanged) keys, if we wanted them
partitionInsertResults :: [DT.Text] -> [DT.Text] -> ([DT.Text], [DT.Text])
partitionInsertResults sourceDataKeysList dataRowsAlready = (minus dataRowsAlready sourceDataKeysList, minus sourceDataKeysList dataRowsAlready)


expandViewList :: [ViewId] -> RowResult (UserId, ViewId)
expandViewList viewList = userList >>= fmap concat . sequence . map (thisViewByUser viewList)


thisViewByUser :: [ViewId] -> UserId -> RowResult (UserId, ViewId)
thisViewByUser views user = map (viewsPerUser user) `fmap` userDeckEndsViewed user views


viewsPerUser :: UserId -> Entity UserDeckEnd -> (UserId, ViewId)
viewsPerUser user (Entity _ view) = (user, userDeckEndViewId view)


-- fields are:  opened handle/connection field_list key_list row_keys
data OpenDataSource = OpenDataSource DataHandle [DT.Text] [DT.Text] [DT.Text]


-- Given a data-source code, open it and return the required parameters.
connection :: JRState.JRState -> [DT.Text] -> IO (Either DT.Text OpenDataSource)

connection site [ "P", serverIP, maybePort, maybeDBase, dataTable, maybeUsername, maybePassword ] = catch (connectPostgreSQL connString >>= pullStructure) sourceFail where

	connString = "host=" ++ DT.unpack serverIP
		++ maybe "" (makeLabel "port") (maybeIntValue maybePort)
		++ (if DT.null maybeDBase then "" else makeLabel "dbname" (DT.unpack maybeDBase))
		++ " user=" ++ (DT.unpack $ if DT.null maybeUsername then JRState.databaseUser site else maybeUsername)
		++ (if DT.null maybePassword then "" else makeLabel "password" (DT.unpack maybePassword))

	pullStructure conn = HDBC.describeTable conn tableNameStr >>= mashIntoFields conn

	mashIntoFields conn rows = HDBC.prepare conn primyKeyQuery
		>>= exeStmt
		>>= return . maybe [] (map DT.pack) . sequence . map head
		>>= kazam conn rows

	kazam conn rows primaryKey = HDBC.prepare conn ("SELECT " ++ primyKeysForQ primaryKey ++ " FROM \"" ++ tableNameStr ++ "\";")
		>>= exeStmt
		-- extract primary key value from row as [[Maybe String]] and convert to [DataRow]
		-- TODO do something useful if the key be Nothing;  i.e., if any key field be Nothing
		>>= return . map (enSerialise . map DT.pack) . catMaybes . map sequence
		>>= return . Right . OpenDataSource (Postgres conn dataTable) (map putColHead rows) primaryKey

	primyKeyQuery = "SELECT \"attname\" FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = '\"" ++ tableNameStr ++ "\"'::regclass ORDER BY a.attnum;"

	tableNameStr = DT.unpack dataTable


connection _site [ "Q", _dtableName ] = return $ Left "SQLite not implemented yet"

connection _site [ "C", _recseparator, _ffileCSV ] = return $ Left "CSV not implemented yet"

connection _site [ "X", _ffileXML ] = return $ Left "XML not implemented yet"

connection _ connStr = return $ Left $ DT.concat ("invalid connection string: " : connStr)


sourceFail :: HDBC.SqlError -> IO (Either DT.Text OpenDataSource)
sourceFail err = return $ Left $ DT.concat ["SQL error: ", DT.pack $ show err]


makeLabel :: Show a => String -> a -> String
makeLabel label value = " " ++ label ++ "=\"" ++ show value ++ "\""


-- comma-separated list of primary key fieldsKey
primyKeysForQ :: [DT.Text] -> String
primyKeysForQ = DT.unpack . DT.concat . intersperse "," . map enQuote


-- TODO actually look at the Integer returned and throw an error if necessary
exeStmt :: Statement -> IO [[Maybe String]]
exeStmt stmt = HDBC.sExecute stmt [] >> HDBC.sFetchAllRows stmt


putColHead :: (String, HDBC.SqlColDesc) -> DT.Text
putColHead = DT.pack . fst


enQuote :: DT.Text -> DT.Text
enQuote word = DT.concat [packedQuote, word, packedQuote]


packedQuote :: DT.Text
packedQuote = "\""
