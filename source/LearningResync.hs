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


import Data.Time (getCurrentTime)
import Control.Exception (catch)
import Data.List (foldl', intersperse)
import qualified Data.Map as DM (insert)
import qualified Data.Text as DT (Text, concat, pack, unpack, null)
import Database.Persist.Sql (Entity, Key)
import Database.Persist.Sqlite (runSqlPool, selectList)
import Database.Persist (replace)
import Database.Persist.Sql (insertBy)
import Database.Persist.Types (entityKey, entityVal)
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
import Debug (display)


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

updateOneSource site schemeMap sourceRecord = liftIO (connection site (deSerialise dataSourceString)) >>= either badConnString reSyncRows where

	badConnString word = logWarnNS dataNameString (DT.concat [dataSourceString, ": error: ", word]) >> schemeMap

	reSyncRows :: OpenDataSource -> LoggingT IO DataSchemes
	reSyncRows (OpenDataSource openConn colheads _ dataKeysList) = liftIO getCurrentTime >>= updateSync
				>> schemeMap >>= liftIO . return . (:) (dataSourceKey, DataDescriptor colheads dataSourceKey openConn) where
		updateSync timeStamp = runSqlPool (sequence $ map insertRow dataKeysList) learningPersistPool >>= updateSource . DE.partitionEithers where
			insertRow keyValue = insertBy $ LearningData.DataRow keyValue dataSourceKey timeStamp
			-- Update time-stamp only replace if new data_rows were inserted.
			updateSource :: ([Entity LearningData.DataRow], [Key LearningData.DataRow]) -> LoggingT IO ()
			updateSource (_, []) = return ()
			updateSource (olds, newItems) = runSqlPool (userList >>= fmap concat . sequence . map viewByUser >>= fmap concat . sequence . map insertLearnDatum) learningPersistPool >>
					runSqlPool (replace dataSourceKey dataSourceParts{LearningData.dataSourceResynced = timeStamp}) learningPersistPool where
				viewByUser :: UserId -> Control.Monad.Trans.Reader.ReaderT Database.Persist.Sql.SqlBackend (LoggingT IO) [(UserId, LearningData.ViewId)]
				viewByUser user = (map (\view -> (user, userDeckEndViewId $ entityVal view))) `fmap` userDeckEnds user
				insertLearnDatum :: (UserId, LearningData.ViewId) -> Control.Monad.Trans.Reader.ReaderT Database.Persist.Sql.SqlBackend (LoggingT IO) [Either (Entity LearningData.LearnDatum) (Key LearningData.LearnDatum)]
				insertLearnDatum (user, view) = sequence $ map (\itemId -> insertBy $ LearningData.mkLearnDatum view itemId user 0 timeStamp) newItems

	learningPersistPool = JRState.tablesFile site
	dataSourceKey = entityKey sourceRecord
	dataSourceParts = entityVal sourceRecord
	dataSourceString = LearningData.dataSourceSourceSerial dataSourceParts
	dataNameString = LearningData.dataSourceName dataSourceParts


-- fields are:  opened handle/connection field_list key_list row_keys
data OpenDataSource = OpenDataSource DataHandle [DT.Text] [DT.Text] [DT.Text]


-- Given a data-source code, open it and return the required parameters.
connection :: JRState.JRState -> [DT.Text] -> IO (Either DT.Text OpenDataSource)

connection site [ "P", serverIP, maybePort, maybeDBase, dataTable, maybeUsername, maybePassword ] = catch (neoConn >>= pullStructure) sourceFail where

	neoConn = connectPostgreSQL $
			"host=" ++ DT.unpack serverIP ++ "" ++
			maybe "" (makeLabel " port") (maybeIntValue maybePort) ++
			(if DT.null maybeDBase then "" else makeLabel " dbname" (DT.unpack maybeDBase)) ++
			" user=" ++ (DT.unpack $ if DT.null maybeUsername then JRState.databaseUser site else maybeUsername) ++
			(if DT.null maybePassword then "" else makeLabel " password" (DT.unpack maybePassword))

	pullStructure conn = HDBC.describeTable conn tableNameStr >>= mashIntoFields conn

	mashIntoFields conn rows = HDBC.prepare conn primyKeyQuery >>=
			exeStmt >>=
			return . maybe [] (map DT.pack) . sequence . map head >>=
			kazam where

			kazam primaryKey = HDBC.prepare conn primyKeyListQuery
					>>= exeStmt
					-- extract primary key value from row as [[Maybe String]] and convert to [DataRow]
					-- TODO do something useful if the key be Nothing;  i.e., if any key field be Nothing
					>>= return . map (enSerialise . map DT.pack) . catMaybes . map sequence
					>>= return . Right . OpenDataSource (Postgres conn dataTable) (map putColHead rows) primaryKey where

				-- SQL to select primary key data rows
				primyKeyListQuery = "SELECT " ++ primyKeysForQ ++ " FROM \"" ++ DT.unpack dataTable ++ "\";"

				-- comma-separated list of primary key fieldsKey
				primyKeysForQ = DT.unpack $ DT.concat $ intersperse packedComma $ map enQuote primaryKey

	sourceFail :: HDBC.SqlError -> IO (Either DT.Text OpenDataSource)
	sourceFail err = return $ Left $ DT.concat ["SQL error: ", DT.pack $ show err]

	primyKeyQuery = "SELECT \"" ++ attName ++ "\" FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = '\"" ++ tableNameStr ++ "\"'::regclass ORDER BY a.attnum;"

	tableNameStr = DT.unpack dataTable

	makeLabel label value = label ++ "=\"" ++ show value ++ "\""


connection _site [ "Q", _dtableName ] = return $ Left "SQLite not implemented yet"

connection _site [ "C", _recseparator, _ffileCSV ] = return $ Left "CSV not implemented yet"

connection _site [ "X", _ffileXML ] = return $ Left "XML not implemented yet"

connection _ connStr = return $ Left $ DT.concat ("invalid connection string: " : connStr)


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
packedComma = ","
packedQuote = "\""
