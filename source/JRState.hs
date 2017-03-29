{-|
Description: Yesod Foundation type
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module JRState where


import Database.Persist.Sql (ConnectionPool)
import Authorisation (UserId)
import Data.Text (Text)
import UserDeck (UserDeckCpt)
import Data.Map (Map)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad.Logger (LoggingT, LogLevel)
import Control.Monad.IO.Class (MonadIO)
import LogFilter (runFilteredLoggingT)
import ConnectionSpec (DataDescriptor, PostgresConnection)
import LearningData (DataSourceId)
import Database.HDBC.PostgreSQL (Connection)


type PostgresConnPool = Map PostgresConnection (Int, Connection)


type DataSchemes = Map DataSourceId DataDescriptor


-- the key of the map is Text because the security subsystem returns a user id.
type UserConfig = Map Text (UserId, [UserDeckCpt])


-- | The foundation object
data JRState = JRState {
		logLevel :: LogLevel,
			-- ^ detail of diagnostics
		tablesFile :: ConnectionPool,
			-- ^ name of file containing SQLite3 tables
		secureOnly :: Bool,
			-- ^ restrict connections to HTTPS
		shuffleCards :: Bool,
			-- ^ default value for shuffling of per-user reviews
		sessionTimeout :: Int,
			-- ^ in minutes
		portNumber :: Maybe Int,
			-- ^ useful to override for non-privileged testing
		keysFile :: FilePath,
			-- ^ AES keys
		databaseUser :: Text,
			-- ^ default account with which to open database source tables.
		appRoot :: Text,
			-- ^ needed for identification emails
		postgresConnections :: TVar PostgresConnPool,
			-- ^ all tables in a given database share a common PG ConnectionPool
		dataSchemes :: TVar DataSchemes,
			-- ^ 'live' LearningData.DataSource with opened connections
		userConfig :: TVar UserConfig
			-- ^ one per logged-in user
	}


runFilteredLoggingT :: MonadIO m => JRState -> LoggingT m a -> m a
runFilteredLoggingT site = LogFilter.runFilteredLoggingT (logLevel site)


getPostgresConnPool :: JRState -> IO PostgresConnPool
getPostgresConnPool = readTVarIO . postgresConnections


getDataSchemes :: JRState -> IO DataSchemes
getDataSchemes = readTVarIO . dataSchemes


getUserConfig :: JRState -> IO UserConfig
getUserConfig = readTVarIO . userConfig
