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
import qualified Data.Map as DM (insert)
import qualified Data.Text as DT (Text, concat, pack, unpack, null)
import Database.Persist.Sql (Entity(Entity), Key)
import Database.Persist.Sqlite (runSqlPool, selectList)
import Database.Persist (replace)
import Database.Persist.Sql (insertBy)
import TextList (enSerialise)
import Data.Maybe (catMaybes)
import qualified JRState (JRState(..), runFilteredLoggingT)
import qualified LearningData (DataSource(..), DataRow(..), DataSourceId, LearnDatum, mkLearnDatum, ViewId)
import qualified Database.HDBC as HDBC (SqlError, SqlColDesc, describeTable, prepare, sExecute, sFetchAllRows)
import Database.HDBC.Statement (Statement)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logWarnNS)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import ConnectionSpec (DataDescriptor(..), DataHandle(..))
import MaybeIntValue (maybeIntValue)
import TextList (deSerialise)
import qualified Data.Either as DE (partitionEithers)
import DeckData (userDeckEnds, userDeckEndViewId)
import Authorisation (UserId, userList)
import qualified Control.Monad.Trans.Reader (ReaderT)
import qualified Database.Persist.Sql (SqlBackend)


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


type DataSchemes = LoggingT IO [(LearningData.DataSourceId, DataDescriptor)]


type RowResult a = Control.Monad.Trans.Reader.ReaderT Database.Persist.Sql.SqlBackend (LoggingT IO) [a]


updateOneSource :: JRState.JRState -> DataSchemes -> Entity LearningData.DataSource -> DataSchemes

updateOneSource site schemeMap sourceRecord = liftIO (connection site (deSerialise dataSourceString)) >>= either badConnString reSyncRows where

	badConnString word = logWarnNS dataNameString (DT.concat [dataSourceString, ": error: ", word]) >> schemeMap

	reSyncRows :: OpenDataSource -> DataSchemes
	reSyncRows (OpenDataSource openConn colheads _ dataKeysList) = liftIO getCurrentTime
		>>= updateSync dataKeysList
		>> schemeMap
		>>= liftIO . return . (:) (dataSourceKey, DataDescriptor colheads dataSourceKey openConn)

	updateSync dataKeysList timeStamp = runSqlPool ((sequence $ map (insertRow timeStamp) dataKeysList)
		>>= (updateSource timeStamp) . DE.partitionEithers) (JRState.tablesFile site)

	insertRow timeStamp keyValue = insertBy $ LearningData.DataRow keyValue dataSourceKey timeStamp

	-- Update time-stamp only if new data_rows were inserted.
	updateSource :: UTCTime -> ([Entity LearningData.DataRow], [Key LearningData.DataRow]) -> RowResult (Either (Entity LearningData.LearnDatum) (Key LearningData.LearnDatum))
	updateSource _ (_, []) = return []
	updateSource timeStamp (olds, newItems) = replace dataSourceKey dataSourceParts{LearningData.dataSourceResynced = timeStamp}
		>> userList
		>>= fmap concat . sequence . map viewByUser
		>>= fmap concat . sequence . map (insertLearnDatum timeStamp newItems)

	viewByUser :: UserId -> RowResult (UserId, LearningData.ViewId)
	viewByUser user = map (viewsPerUser user) `fmap` userDeckEnds user

	viewsPerUser user (Entity _ view) = (user, userDeckEndViewId view)

	insertLearnDatum :: UTCTime -> [Key LearningData.DataRow] -> (UserId, LearningData.ViewId) -> RowResult (Either (Entity LearningData.LearnDatum) (Key LearningData.LearnDatum))
	insertLearnDatum timeStamp newItems (user, view) = sequence $ map (\itemId -> insertBy $ LearningData.mkLearnDatum view itemId user 0 timeStamp) newItems

	(Entity dataSourceKey dataSourceParts) = sourceRecord
	dataSourceString = LearningData.dataSourceSourceSerial dataSourceParts
	dataNameString = LearningData.dataSourceName dataSourceParts


-- fields are:  opened handle/connection field_list key_list row_keys
data OpenDataSource = OpenDataSource DataHandle [DT.Text] [DT.Text] [DT.Text]


-- Given a data-source code, open it and return the required parameters.
connection :: JRState.JRState -> [DT.Text] -> IO (Either DT.Text OpenDataSource)

connection site [ "P", serverIP, maybePort, maybeDBase, dataTable, maybeUsername, maybePassword ] = catch (connectPostgreSQL connString >>= pullStructure) sourceFail where

	connString = "host=" ++ DT.unpack serverIP ++ ""
		++ maybe "" (makeLabel " port") (maybeIntValue maybePort)
		++ (if DT.null maybeDBase then "" else makeLabel " dbname" (DT.unpack maybeDBase))
		++ " user=" ++ (DT.unpack $ if DT.null maybeUsername then JRState.databaseUser site else maybeUsername)
		++ (if DT.null maybePassword then "" else makeLabel " password" (DT.unpack maybePassword))

	pullStructure conn = HDBC.describeTable conn tableNameStr >>= mashIntoFields conn

	mashIntoFields conn rows = HDBC.prepare conn primyKeyQuery
		>>= exeStmt
		>>= return . maybe [] (map DT.pack) . sequence . map head
		>>= kazam conn rows

	kazam conn rows primaryKey = HDBC.prepare conn ("SELECT " ++ primyKeysForQ primaryKey ++ " FROM \"" ++ DT.unpack dataTable ++ "\";")
		>>= exeStmt
		-- extract primary key value from row as [[Maybe String]] and convert to [DataRow]
		-- TODO do something useful if the key be Nothing;  i.e., if any key field be Nothing
		>>= return . map (enSerialise . map DT.pack) . catMaybes . map sequence
		>>= return . Right . OpenDataSource (Postgres conn dataTable) (map putColHead rows) primaryKey

	sourceFail :: HDBC.SqlError -> IO (Either DT.Text OpenDataSource)
	sourceFail err = return $ Left $ DT.concat ["SQL error: ", DT.pack $ show err]

	primyKeyQuery = "SELECT \"attname\" FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = '\"" ++ tableNameStr ++ "\"'::regclass ORDER BY a.attnum;"

	tableNameStr = DT.unpack dataTable

	makeLabel label value = label ++ "=\"" ++ show value ++ "\""


connection _site [ "Q", _dtableName ] = return $ Left "SQLite not implemented yet"

connection _site [ "C", _recseparator, _ffileCSV ] = return $ Left "CSV not implemented yet"

connection _site [ "X", _ffileXML ] = return $ Left "XML not implemented yet"

connection _ connStr = return $ Left $ DT.concat ("invalid connection string: " : connStr)


-- comma-separated list of primary key fieldsKey
primyKeysForQ :: [DT.Text] -> String
primyKeysForQ = DT.unpack . DT.concat . intersperse packedComma . map enQuote


-- TODO actually look at the Integer returned and throw an error if necessary
exeStmt :: Statement -> IO [[Maybe String]]
exeStmt stmt = HDBC.sExecute stmt [] >> HDBC.sFetchAllRows stmt


putColHead :: (String, HDBC.SqlColDesc) -> DT.Text
putColHead = DT.pack . fst


enQuote :: DT.Text -> DT.Text
enQuote word = DT.concat [packedQuote, word, packedQuote]


packedComma, packedQuote :: DT.Text
packedComma = ","
packedQuote = "\""
