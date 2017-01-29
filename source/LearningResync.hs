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


module LearningResync (update) where


import Data.Time (getCurrentTime, UTCTime)
import Control.Exception (catch)
import Data.List (foldl', intersperse)
import qualified Data.Map as DM (insert)
import qualified Data.Text as DT (Text, concat, pack, unpack)
import qualified Database.Persist.Class (Key)
import Database.Persist.Sql (Entity)
import Database.Persist.Sqlite (ConnectionPool, runSqlPool, selectList)
import Database.Persist (replace)
import Database.Persist.Sql (insertUnique)
import Database.Persist.Types (entityKey, entityVal)
import DataSource (deSerialise)
import TextList (enSerialise)
import Data.Maybe (fromMaybe)
import qualified JRState (JRState(..), runFilteredLoggingT)
import qualified LearningData (DataSource(..), DataRow(..), DataSourceId)
import qualified DataSource as DS (DataVariant(..))
import qualified Database.HDBC as HDBC (SqlError, SqlColDesc, describeTable, prepare, sExecute, sFetchAllRows)
import Database.HDBC.Statement (Statement)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logWarnNS)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import ConnectionSpec (DataDescriptor(..), DataHandle(..))


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


type DataSchemes = [(LearningData.DataSourceId, DataDescriptor)]


updateOneSource :: JRState.JRState -> LoggingT IO DataSchemes -> Entity LearningData.DataSource -> LoggingT IO DataSchemes

updateOneSource site schemeMap sourceRecord = maybe badConnString mkConnection (deSerialise dataSourceString) where

	badConnString = logWarnNS dataNameString (DT.concat [DT.pack "Invalid data source: ", dataSourceString]) >> schemeMap

	mkConnection :: DS.DataVariant -> LoggingT IO DataSchemes
	mkConnection connClass = liftIO (connection site connClass) >>= reSyncRows

	reSyncRows :: OpenDataSource -> LoggingT IO DataSchemes
	reSyncRows (Unavailable justification) = logWarnNS dataNameString justification >> schemeMap
	reSyncRows (OpenDataSource openConn colheads priKeys) = liftIO getCurrentTime >>= updateSync
				>> schemeMap >>= liftIO . return . (:) (dataSourceKey, DataDescriptor colheads dataSourceKey openConn) where
		updateSync timeStamp = liftIO (reSyncOneSource openConn site colheads timeStamp priKeys dataSourceKey learningPersistPool) >>= updateSource timeStamp
		-- Update time-stamp only replace if new data_rows were inserted.
		-- It looks like we could use just a Boolean, but the foldl' below stops when True be returned
		updateSource _ 0 = return ()
		updateSource timeStamp _ = runSqlPool (replace dataSourceKey dataSourceParts{LearningData.dataSourceResynced = timeStamp}) learningPersistPool

	learningPersistPool = JRState.tablesFile site
	dataSourceKey = entityKey sourceRecord
	dataSourceParts = entityVal sourceRecord
	dataSourceString = LearningData.dataSourceSourceSerial dataSourceParts
	dataNameString = LearningData.dataSourceName dataSourceParts


data OpenDataSource = OpenDataSource DataHandle [DT.Text] [DT.Text] | Unavailable DT.Text


reSyncOneSource :: DataHandle -> JRState.JRState -> [DT.Text] -> UTCTime -> [DT.Text] -> Database.Persist.Class.Key LearningData.DataSource -> ConnectionPool -> IO Int

reSyncOneSource (Postgres sourceConn sourceDBtable) site _ timeStamp primaryKey sourceKey learningPersistPool =
	HDBC.prepare sourceConn primyKeyQuery >>= exeStmt >>= foldl' row1yKeyUpdate (return 0) where
		-- extract primary key value from row as [Maybe String] and convert to Maybe [SerialisedKey]
		row1yKeyUpdate :: IO Int -> [Maybe String] -> IO Int
		row1yKeyUpdate mAddCount dataRow = tryInsert mAddCount ((enSerialise . map DT.pack) `fmap` sequence dataRow)
		-- Insert one key value into learning data, if it not already exist therein.
		-- We don't log anything because the Persist calls do it all.
		-- We accume a Boolean which is 'was anything changed?'
		-- TODO try mapM insertBy [rows]
		-- TODO do something useful if the key be Nothing
		tryInsert :: IO Int -> Maybe DT.Text -> IO Int
		tryInsert mAddCount = maybe mAddCount (tryInsertKey mAddCount)
		tryInsertKey :: IO Int -> DT.Text -> IO Int
		-- TODO could we return the error code rather than just throwing it away with const mAddCount?
		tryInsertKey mAddCount keyValue = JRState.runFilteredLoggingT site (runSqlPool (insertUnique $ LearningData.DataRow keyValue sourceKey timeStamp) learningPersistPool)
			>>= maybe mAddCount (\_ -> ((+) 1) `fmap` mAddCount)
		-- SQL to select primary key data rows
		primyKeyQuery = "SELECT " ++ primyKeysForQ ++ " FROM \"" ++ DT.unpack sourceDBtable ++ "\";"
		-- comma-separated list of primary key fieldsKey
		primyKeysForQ = DT.unpack $ DT.concat $ intersperse packedComma $ map enQuote primaryKey


-- TODO merge this with DataSource.hs;  there's an unnecessary layer here.
connection :: JRState.JRState -> DS.DataVariant -> IO OpenDataSource

connection site (DS.Postgres serverIP maybePort dbase maybeTable dataTable maybeUser maybePassword) = catch (neoConn >>= pullStructure) sourceFail where
	neoConn = connectPostgreSQL $
			"host=" ++ DT.unpack serverIP ++ "" ++
			maybe "" (makeLabel " port") maybePort ++
			maybe "" (\db -> makeLabel " dbname" (DT.unpack db)) dbase ++
			" user=" ++ (DT.unpack $ fromMaybe (JRState.databaseUser site) maybeUser) ++
			maybe "" (\pw -> makeLabel " password" (DT.unpack pw)) maybePassword
	pullStructure conn = HDBC.describeTable conn tableNameStr >>= mashIntoFields conn
	mashIntoFields conn rows = HDBC.prepare conn primyKeyQuery >>=
			exeStmt >>=
			return . maybe [] (map DT.pack) . sequence . map head >>=
			return . OpenDataSource (Postgres conn dataTable) (map putColHead rows)
	sourceFail :: HDBC.SqlError -> IO OpenDataSource
	sourceFail = return . Unavailable . DT.pack . show
	primyKeyQuery = "SELECT \"" ++ attName ++ "\" FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = '\"" ++ tableNameStr ++ "\"'::regclass ORDER BY a.attnum;"
	tableNameStr = DT.unpack dataTable
	makeLabel label value = label ++ "=\"" ++ show value ++ "\""

connection _ (DS.Sqlite3 dtableName) = return $ OpenDataSource (ConnectionSpec.Sqlite3 dtableName) [] []


-- TODO actually look at the Integer returned and throw an error if necessary
exeStmt :: Statement -> IO [[Maybe String]]
exeStmt stmt = HDBC.sExecute stmt [] >> HDBC.sFetchAllRows stmt


attName :: String
attName = "attname"


putColHead :: (String, HDBC.SqlColDesc) -> DT.Text
putColHead = DT.pack . fst


enQuote :: DT.Text -> DT.Text
enQuote word = DT.concat [packedQuote, word, packedQuote]


packedComma, packedQuote :: DT.Text
packedComma = DT.pack ","
packedQuote = DT.pack "\""
