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
import qualified Data.Text as DT (Text, concat, empty, pack, unpack)
import qualified Database.Persist.Class (Key)
import Database.Persist.Sql (Entity)
import Database.Persist.Sqlite (ConnectionPool, runSqlPool, selectList)
import Database.Persist (replace)
import Database.Persist.Sql (insertBy)
import Database.Persist.Types (entityKey, entityVal)
import DataSource (deSerialise)
import TextList (enSerialise)
import Data.Maybe (fromMaybe)
import qualified JRState (JRState(..), runFilteredLoggingT)
import qualified LearningData (DataSource(..), DataRow(..))
import qualified DataSource as DS (DataVariant(..))
import qualified Database.HDBC as HDBC (SqlError, SqlColDesc, describeTable, prepare, sFetchAllRows)
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logWarnNS)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import ConnectionData (DataDescriptor(..), DataHandle(..))


-- | Create handles or connections to all data sources;  then read them to be sure
-- | all records are known.
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


type DataSchemes = [(DT.Text, ConnectionData.DataDescriptor)]


updateOneSource :: JRState.JRState -> LoggingT IO DataSchemes -> Entity LearningData.DataSource -> LoggingT IO DataSchemes

updateOneSource site schemeMap sourceRecord = maybe badConnString mkConnection (deSerialise dataSourceString) where

	badConnString = logWarnNS dataShortString (DT.concat [DT.pack "Invalid data source: ", dataSourceString]) >> schemeMap

	mkConnection :: DS.DataVariant -> LoggingT IO DataSchemes
	mkConnection connClass = liftIO (connection site connClass) >>= reSyncRows

	reSyncRows :: OpenDataSource -> LoggingT IO DataSchemes
	reSyncRows (Unavailable justification) = logWarnNS dataShortString justification >> schemeMap
	reSyncRows (OpenDataSource openConn colheads priKeys) = liftIO getCurrentTime >>= updateSync
				>> schemeMap >>= liftIO . return . (:) (dataShortString, (DataDescriptor dataLongString colheads dataSourceKey openConn)) where
		updateSync timeStamp = liftIO (reSyncOneSource openConn site colheads timeStamp priKeys dataSourceKey learningPersistPool) >>= updateSource timeStamp
		--update time-stamp only replace if new data_rows were inserted.
		updateSource timeStamp True = runSqlPool (replace dataSourceKey dataSourceParts{LearningData.dataSourceResynced = timeStamp}) learningPersistPool
		updateSource _ False = return ()

	learningPersistPool = JRState.tablesFile site
	dataSourceKey = entityKey sourceRecord
	dataSourceParts = entityVal sourceRecord
	dataSourceString = LearningData.dataSourceSourceSerial dataSourceParts
	dataShortString = LearningData.dataSourceShortName dataSourceParts
	dataLongString = LearningData.dataSourceLongName dataSourceParts


data OpenDataSource = OpenDataSource DataHandle [DT.Text] [DT.Text] | Unavailable DT.Text


reSyncOneSource :: DataHandle -> JRState.JRState -> [DT.Text] -> UTCTime -> [DT.Text] -> Database.Persist.Class.Key LearningData.DataSource -> ConnectionPool -> IO Bool

reSyncOneSource (Postgres sourceConn sourceDBtable) site _ timeStamp primaryKey sourceKey learningPersistPool =
	HDBC.prepare sourceConn primyKeyQuery >>= HDBC.sFetchAllRows >>= foldl' row1yKeyUpdate (return False) where
		-- extract primary key value from row as [Maybe String] and convert to Maybe [SerialisedKey]
		row1yKeyUpdate :: IO Bool -> [Maybe String] -> IO Bool
		row1yKeyUpdate mChanged dataRow = tryInsert mChanged ((enSerialise . map DT.pack) `fmap` sequence dataRow)
		-- Insert one key value into learning data, if it not already exist therein.
		-- We don't log anything because the Persist calls do it all.
		-- We accume a Boolean which is 'was anything changed?'
		-- TODO try mapM insertBy [rows]
		-- TODO do something useful if the key be Nothing
		tryInsert :: IO Bool -> Maybe DT.Text -> IO Bool
		tryInsert mChanged = maybe mChanged (tryInsertKey mChanged)
		tryInsertKey :: IO Bool -> DT.Text -> IO Bool
		-- TODO could we return the error code rather than just throwing it away with const mChanged?
		tryInsertKey mChanged keyValue = JRState.runFilteredLoggingT site (runSqlPool (insertBy (LearningData.DataRow keyValue sourceKey timeStamp)) learningPersistPool)
				>>= either (const mChanged) (return . const True)
		-- SQL to select primary key data rows
		primyKeyQuery = "SELECT " ++ primyKeysForQ ++ " FROM \"" ++ DT.unpack sourceDBtable ++ "\";"
		-- comma-separated list of primary key fields
		primyKeysForQ = DT.unpack $ DT.concat $ intersperse packedComma $ map enQuote primaryKey


connection :: JRState.JRState -> DS.DataVariant -> IO OpenDataSource

connection site (DS.Postgres serverIP maybePort dbase maybeTable dataTable maybeUser maybePassword) = catch (neoConn >>= pullStructure) sourceFail where
	neoConn = connectPostgreSQL $
			"host=" ++ DT.unpack serverIP ++
			maybe "" (makeLabel " port=") maybePort ++
			" dbname=" ++ DT.unpack dbase ++
			" user=" ++ (DT.unpack $ fromMaybe (JRState.databaseUser site) maybeUser) ++
			" password=" ++ (DT.unpack $ fromMaybe DT.empty maybePassword)
	pullStructure conn = HDBC.describeTable conn tableNameStr >>= mashIntoFields conn
	mashIntoFields conn rows = HDBC.prepare conn primyKeyQuery >>= HDBC.sFetchAllRows >>=return . maybe [] (map DT.pack) . sequence . map head >>= return . zing where
		zing primKeys = OpenDataSource (Postgres conn dataTable) (map putColHead rows) primKeys
	sourceFail :: HDBC.SqlError -> IO OpenDataSource
	sourceFail = return . Unavailable . DT.pack . show
	primyKeyQuery = "SELECT \"" ++ attName ++ "\" FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = '\"" ++ tableNameStr ++ "\"'::regclass ORDER BY a.attnum;"
	tableNameStr = DT.unpack dataTable
	makeLabel label value = label ++ show value

connection _ (DS.Sqlite3 dtableName) = return $ OpenDataSource (ConnectionData.Sqlite3 dtableName) [] []


attName :: String
attName = "attname"


putColHead :: (String, HDBC.SqlColDesc) -> DT.Text
putColHead = DT.pack . fst


enQuote :: DT.Text -> DT.Text
enQuote word = DT.concat [packedQuote, word, packedQuote]


packedComma, packedQuote :: DT.Text
packedComma = DT.pack ","
packedQuote = DT.pack "\""
