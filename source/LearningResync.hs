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


import Data.Time (getCurrentTime)
import Persistency (persistAction)
import Control.Exception (catch)
import Data.List (foldl', intersperse)
import qualified Data.Text as DT (Text, concat, empty, pack, unpack)
import qualified Database.Persist.Class (Key)
import Database.Persist.Sql (Entity)
import Database.Persist.Sqlite (ConnectionPool, runSqlPool, selectList)
import Database.Persist (replace)
import Database.Persist.Sql (insertBy)
import Database.Persist.Types (entityKey, entityVal)
import Control.Monad.Logger (runStderrLoggingT)
import DataSource (deSerialise)
import TextList (enSerialise)
import Data.Maybe (fromMaybe)
import qualified JRState (JRState(..))
import qualified LearningData (DataSource(..), DataRow(..))
import qualified DataSource as DS (DataVariant(..))
import qualified Database.HSQL as HSQL (Statement, ColDef, SqlError, describe, Connection, query, forEachRow', collectRows, getFieldValue)
import Database.HSQL.PostgreSQL (connectWithOptions)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugNS, logWarnNS)


-- | Create handles or connections to all data sources;  then read them to be sure
-- all records are known.
update :: JRState.JRState -> IO ()
update site = runStderrLoggingT $ runSqlPool (selectList [] []) (JRState.tablesFile site) >>= foldl' (updateOneSource site) (liftIO $ return ())


updateOneSource :: JRState.JRState -> LoggingT IO () -> Entity LearningData.DataSource -> LoggingT IO ()

updateOneSource site dummy sourceRecord = maybe badConnString mkConnection (deSerialise dataSourceString) where

	badConnString = logWarnNS dataShortString $ DT.concat [DT.pack "Invalid data source: ", dataSourceString]

	mkConnection :: DS.DataVariant -> LoggingT IO ()
	mkConnection connClass = liftIO (connection site connClass) >>= reSyncRows

	reSyncRows :: OpenDataSource -> LoggingT IO ()
	reSyncRows (Unavailable justification) = logWarnNS dataShortString justification
	reSyncRows (OpenDataSource openConn colheads priKeys) = liftIO (reSyncOneSource openConn colheads priKeys dataSourceKey learningPersistPool)
			>> runSqlPool (replace dataSourceKey dataSourceParts) learningPersistPool >> dummy

	learningPersistPool = JRState.tablesFile site
	dataSourceKey = entityKey sourceRecord
	dataSourceParts = entityVal sourceRecord
	dataSourceString = LearningData.dataSourceSourceSerial dataSourceParts
	dataShortString = LearningData.dataSourceShortName dataSourceParts


data OpenDataSource = OpenDataSource OpenConnection [DT.Text] [DT.Text] | Unavailable DT.Text


data OpenConnection = Postgres HSQL.Connection DT.Text | Sqlite3 DT.Text | CSV Char DT.Text | XMLSource DT.Text


reSyncOneSource :: OpenConnection -> [DT.Text] -> [DT.Text] -> Database.Persist.Class.Key LearningData.DataSource -> ConnectionPool -> IO ()

reSyncOneSource (Postgres sourceConn sourceDBtable) _ primaryKey sourceKey learningPersistPool = HSQL.query sourceConn primyKeyQuery >>= HSQL.forEachRow' row1yKeyUpdate where
	-- extract primary key value from row, add timestamp and attempt insertion
	row1yKeyUpdate stmt = row1yKeyValue stmt >>= \keyValue -> getCurrentTime >>= tryInsert keyValue
	-- insert one key value into learning data, if it not already exist therein
	-- we don't log anything because the Persist calls do it all
	tryInsert keyValue timeStamp = runStderrLoggingT (persistAction (insertBy (LearningData.DataRow keyValue sourceKey timeStamp)) learningPersistPool >> return ()
	-- primary key fields serialised into one Text
	row1yKeyValue stmt = enSerialise `fmap` mapM (columnValue stmt) primaryKey
	-- SQL to select primary key data rows
	primyKeyQuery = "SELECT " ++ primyKeysForQ ++ " FROM \"" ++ DT.unpack sourceDBtable ++ "\";"
	-- comma-separated list of primary key fields
	primyKeysForQ = DT.unpack $ DT.concat $ intersperse packedComma $ map enQuote primaryKey


columnValue :: HSQL.Statement -> DT.Text -> IO DT.Text
columnValue stmt colName = DT.pack `fmap` HSQL.getFieldValue stmt (DT.unpack colName)


connection :: JRState.JRState -> DS.DataVariant -> IO OpenDataSource

connection site (DS.Postgres serverIP maybePort dbase maybeTable dataTable maybeUser maybePassword) = catch (neoConn >>= pullStructure) sourceFail where
	neoConn = connectWithOptions
			(DT.unpack serverIP)
			(fmap show maybePort)
			Nothing
			Nothing
			(DT.unpack dbase)
			(DT.unpack $ fromMaybe (JRState.databaseUser site) maybeUser)
			(DT.unpack $ fromMaybe DT.empty maybePassword)
	pullStructure conn = HSQL.describe conn tableNameStr >>= mashIntoFields conn
	mashIntoFields conn rows = primaryKey conn >>= (\py -> return $ OpenDataSource (Postgres conn dataTable) (map putColHead rows) py)
	sourceFail :: HSQL.SqlError -> IO OpenDataSource
	sourceFail = return . Unavailable . DT.pack . show
	primyKeyQuery = "SELECT \"" ++ attName ++ "\" FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = '\"" ++ tableNameStr ++ "\"'::regclass AND i.indisprimary ORDER BY a.attnum;"
	tableNameStr = DT.unpack dataTable
	primaryKey :: HSQL.Connection -> IO [DT.Text]
	primaryKey conn = HSQL.query conn primyKeyQuery >>= HSQL.collectRows (flip columnValue attNameP)

connection _ (DS.Sqlite3 dtableName) = return $ OpenDataSource (Sqlite3 dtableName) [] []

connection _ (DS.CSV recSep ffileCSV) = return $ OpenDataSource (CSV recSep ffileCSV) [] []

connection _ (DS.XMLSource ffileXML) = return $ OpenDataSource (XMLSource ffileXML) [] []


attName :: String
attName = "attname"


attNameP :: DT.Text
attNameP = DT.pack attName


putColHead :: HSQL.ColDef -> DT.Text
putColHead (colId, _, _) = DT.pack colId


enQuote :: DT.Text -> DT.Text
enQuote word = DT.concat [packedQuote, word, packedQuote]


packedComma, packedQuote :: DT.Text
packedComma = DT.pack ","
packedQuote = DT.pack "\""
