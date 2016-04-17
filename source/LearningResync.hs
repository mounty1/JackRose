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


module LearningResync (update, Connection(..), DataSource(..)) where


import qualified Data.Text as DT (Text, empty, pack, unpack)
import Database.Persist.Sqlite (runSqlPool, selectList)
import Database.Persist (replace)
import Database.Persist.Types (entityKey, entityVal)
import Control.Monad.Logger (runStderrLoggingT, MonadLogger, logInfoN)
import DataSource (deSerialise)
import Data.Maybe (fromMaybe)
import qualified JRState (JRState(..))
import qualified LearningData (DataSource(..))
import qualified DataSource as DS (DataVariant(..))
import qualified Database.HSQL as HSQL (ColDef, describe, Connection)
import Database.HSQL.PostgreSQL (connectWithOptions)


update :: JRState.JRState -> IO ()
update site = runStderrLoggingT $ runSqlPool (selectList [] []) pool >>= mapM_ updateOneSource where
	updateOneSource entity = resyncOneSource site (entityVal entity) >>= updateSourceRecord (entityKey entity)
	updateSourceRecord entyKey afterSource = runSqlPool (replace entyKey afterSource) pool
	pool = JRState.tablesFile site


resyncOneSource :: MonadLogger m => JRState.JRState -> LearningData.DataSource -> m LearningData.DataSource
resyncOneSource site sourceRecord = logInfoN (DT.pack "SYNCING")
		>> return sourceRecord where
	maybeSourceHandle = fmap (connection site) (deSerialise $ LearningData.dataSourceSourceSerial sourceRecord)


data DataSource = DataSource {
		source :: Connection,
		fields :: [DT.Text]
	}


data Connection = Postgres { connectionSQL :: HSQL.Connection, table :: DT.Text }
		| Sqlite3 { tableName :: DT.Text }
		| CSV { separator :: Char, fileCSV :: DT.Text }
		| XMLSource { fileXML :: DT.Text }


connection :: JRState.JRState -> DS.DataVariant -> IO DataSource
connection site (DS.Postgres serverIP maybePort dbase maybeTable dataTable maybeUser maybePassword) =
		connectWithOptions (DT.unpack serverIP) (fmap show maybePort) Nothing Nothing (DT.unpack dbase) (DT.unpack $ fromMaybe (JRState.databaseUser site) maybeUser) (DT.unpack $ fromMaybe DT.empty maybePassword) >>= pullStructure where
			pullStructure conn = HSQL.describe conn (DT.unpack dataTable) >>= mashIntoFields where
				mashIntoFields rows = return $ DataSource (Postgres conn dataTable) (map putColHead rows)
			-- primyKeysQuery = "SELECT a.attname FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = '\"" ++ DT.unpack dataTable ++ "\"'::regclass AND i.indisprimary ORDER BY a.attnum;"

connection _ (DS.Sqlite3 dtableName) = return $ DataSource (Sqlite3 dtableName) []

connection _ (DS.CSV recSep ffileCSV) = return $ DataSource (CSV recSep ffileCSV) []

connection _ (DS.XMLSource ffileXML) = return $ DataSource (XMLSource ffileXML) []

putColHead :: HSQL.ColDef -> DT.Text
putColHead (colId, _, _) = DT.pack colId


-- main = catch (connectWithOptions "services" Nothing Nothing Nothing "mounty" "mounty" "--------" >>= actions) handler


-- handler :: SqlError -> IO ()
-- handler err = print $ "Oh no: " ++ show err
