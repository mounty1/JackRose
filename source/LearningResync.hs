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
import Data.List (foldl', intersperse, (\\))
import qualified Database.HDBC as HDBC (SqlError, SqlColDesc, describeTable)
import qualified Data.Map as DM (insert, adjust, lookup)
import qualified Data.Text as DT (Text, concat, pack, unpack, empty, null)
import qualified JRState (JRState(..), getPostgresConnPool, runFilteredLoggingT)
import Database.Persist.Sql (SqlBackend, Entity(Entity), Key, runSqlPool, selectList, insertBy, fromSqlKey)
import Database.Persist (replace)
import TextList (enSerialise)
import Data.Maybe (catMaybes)
import LearningData (DataSource(..), DataRow(..), DataSourceId, LearnDatum(..), mkLearnDatum, ViewId, deleteItems, allSourceKeys, viewsOnDataSource)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoNS, logWarnNS, logErrorNS)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import ConnectionSpec (DataDescriptor(..), DataHandle(..), PostgresConnection(..))
import MaybeIntValue (maybeIntValue)
import TextList (deSerialise)
import DeckData (userDeckEndsViewed, userDeckEndViewId, UserDeckEnd)
import Authorisation (UserId, userList)
import Control.Monad.Trans.Reader (ReaderT)
import TextShow (showt)
import ExecuteSqlStmt (exeStmt)
import Data.Bifunctor (first)


-- | Create handles or connections to all data sources;
-- then read them to be sure all records are known.
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
	reSyncRows (OpenDataSource openConn colheads primyKeys dataKeysList) = liftIO getCurrentTime
		>>= updateSync dataKeysList
		>> schemeMap
		>>= liftIO . return . (:) (dataSourceId, DataDescriptor colheads primyKeys openConn)

	updateSync dataKeysList timeStamp = runSqlPool
			(allSourceKeys dataSourceId >>= updateSource timeStamp . partitionInsertResults dataKeysList)
			(JRState.tablesFile site)

	-- Update time-stamp only if new data_rows were inserted;  result is new rows inserted.
	updateSource :: UTCTime -> ([DT.Text], [DT.Text]) -> RowResult (Key LearnDatum)
	updateSource _ ([], []) = return []
	updateSource timeStamp (deleted, newItems) = replace dataSourceId dataSourceParts{dataSourceResynced = timeStamp}
		>> logKeyDelta "deleting" deleted
		>> deleteItems deleted
		>> logKeyDelta "adding" newItems
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
	insertLearnDatum timeStamp itemId (user, view) = insertBy(mkLearnDatum view itemId user 0 1 0.3 86400 timeStamp) >>= either alreadyDatumHuh (return . return)

	-- as above, this should not happen.
	alreadyDatumHuh (Entity _ (LearnDatum vId _ _ _ _ _ _ _)) = alreadyRowPresent "learn datum" (showt $ fromSqlKey vId)

	logKeyDelta label rowIds = sequence $ map (\rowId -> logInfoNS dataNameString $ DT.concat [dataSourceString, ": ", label, " \"", rowId, "\""]) rowIds

	alreadyRowPresent label rowId = logErrorNS dataNameString (DT.concat [dataSourceString, ": ", label, " \"", rowId, "\" already exists"]) >> return []

	dataSourceString = dataSourceSourceSerial dataSourceParts
	dataNameString = dataSourceName dataSourceParts


-- This is where we would call isect to get the common (unchanged) keys, if we wanted them.
-- It is tempting to use Data.List.Ordered.minus instead of Data.List.(\\) but it seems not to work,
-- probably over disagreement over collation arising from differences in character encoding.
partitionInsertResults :: [DT.Text] -> [DT.Text] -> ([DT.Text], [DT.Text])
partitionInsertResults sourceDataKeysList dataRowsAlready = (dataRowsAlready \\ sourceDataKeysList, sourceDataKeysList \\ dataRowsAlready)


expandViewList :: [ViewId] -> RowResult (UserId, ViewId)
expandViewList viewList = userList >>= fmap concat . sequence . map (thisViewByUser viewList)


thisViewByUser :: [ViewId] -> UserId -> RowResult (UserId, ViewId)
thisViewByUser views user = map (viewsPerUser user) `fmap` userDeckEndsViewed user views


viewsPerUser :: UserId -> Entity UserDeckEnd -> (UserId, ViewId)
viewsPerUser user (Entity _ view) = (user, userDeckEndViewId view)


-- fields are:  opened handle/connection field_list key_list row_keys
data OpenDataSource = OpenDataSource DataHandle [DT.Text] [DT.Text] [DT.Text]


-- Given a data-source code, open it and return the required parameters.
-- Since data-sources are given in terms of a database connection and a table, it is quite likely that
-- several connections will share one connection.  So this function either re-uses the existing
-- connection (and increases its reference count) or adds a new connection to the JRState pool.
connection :: JRState.JRState -> [DT.Text] -> IO (Either DT.Text OpenDataSource)

connection site [ "P", serverIP, maybePortNo, maybeDBase, dataTable, maybeUsername, maybePassw ] = catch (JRState.getPostgresConnPool site
			>>= maybe (connectPostgreSQL connString >>= newConn) reuseConn . DM.lookup connectionParms
			>>= pullStructure) (sourceFail connString) where

	connString = "host=" ++ DT.unpack svr
		++ maybeMakeLabel "port" (fmap (DT.pack . show) mbP)
		++ maybeMakeLabel "dbname" mbD
		++ " user=" ++ DT.unpack uName
		++ maybeMakeLabel "password" mbW

	connectionParms@(PostgresConnection svr mbP mbD uName mbW) = PostgresConnection
			serverIP
			(maybeIntValue maybePortNo)
			(if DT.null maybeDBase then Nothing else Just maybeDBase)
			(if DT.null maybeUsername then JRState.databaseUser site else maybeUsername)
			(if DT.null maybePassw then Nothing else Just maybePassw)

	-- This database connection doesn't exist yet in the pool so add it.
	newConn conn = produceConn conn (DM.insert connectionParms (1, conn))

	-- This database connection does exist so merely increment its reference count.
	reuseConn (_, conn) = produceConn conn (DM.adjust (first (1 +)) connectionParms)

	-- Workhorse for the newConn and reuseConn
	produceConn conn fn = atomically (modifyTVar' (JRState.postgresConnections site) fn) >> return conn

	pullStructure conn = HDBC.describeTable conn tableNameStr >>= mashIntoFields conn

	mashIntoFields conn rows = exeStmt conn primyKeyQuery []
		>>= return . maybe [] (map DT.pack) . sequence . map head
		>>= kazam conn rows

	kazam conn rows primaryKey = exeStmt conn ("SELECT " ++ keysList ++ " FROM \"" ++ tableNameStr ++ "\" ORDER BY " ++ keysList ++ ";") []
		-- extract primary key value from row as [[Maybe String]] and convert to [DataRow]
		-- TODO do something useful if the key be Nothing;  i.e., if any key field be Nothing
		>>= return . map (enSerialise . map DT.pack) . catMaybes . map sequence
		>>= return . Right . OpenDataSource (Postgres conn dataTable) (map putColHead rows) primaryKey where
			keysList = primyKeysForQ primaryKey

	primyKeyQuery = "SELECT \"attname\" FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = '\"" ++ tableNameStr ++ "\"'::regclass AND i.indisprimary ORDER BY a.attnum;"

	tableNameStr = DT.unpack dataTable

connection _site [ "Q", _dtableName ] = return $ Left "SQLite not implemented yet"

connection _site [ "C", _recseparator, _ffileCSV ] = return $ Left "CSV not implemented yet"

connection _site [ "X", _ffileXML ] = return $ Left "XML not implemented yet"

connection _ connStr = return $ Left $ DT.concat ("invalid connection string: " : connStr)


sourceFail :: String -> HDBC.SqlError -> IO (Either DT.Text OpenDataSource)
sourceFail connString sqlError = return $ Left $ DT.pack $ concat ["\"", connString, "\":", show sqlError]


maybeMakeLabel :: DT.Text -> Maybe DT.Text -> String
maybeMakeLabel label = DT.unpack . maybe DT.empty (\value -> DT.concat [ " ", label, "=", value ])


-- comma-separated list of primary key fieldsKey
primyKeysForQ :: [DT.Text] -> String
primyKeysForQ = DT.unpack . DT.concat . intersperse "," . map enQuote


putColHead :: (String, HDBC.SqlColDesc) -> DT.Text
putColHead = DT.pack . fst


enQuote :: DT.Text -> DT.Text
enQuote word = DT.concat [packedQuote, word, packedQuote]


packedQuote :: DT.Text
packedQuote = "\""
