{-|
Description: Yesod Foundation type
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module JRState where


import Database.Persist.Sqlite (ConnectionPool)
import qualified AuthoriStyle (Style)
import Data.Text (Text)
import ConfigParse (UserSchema)
import Data.Map (Map)
import Control.Concurrent.STM (TVar)
import Control.Monad.Logger (LoggingT, LogLevel)
import Control.Monad.IO.Class (MonadIO)
import LogFilter (runFilteredLoggingT)


type UserConfig = TVar (Map Text UserSchema)


-- | The foundation object
data JRState = JRState {
		logLevel :: LogLevel,
			-- ^ detail of diagnostics
		tablesFile :: ConnectionPool,
			-- ^ name of file containing SQLite3 tables
		secureOnly :: Bool,
			-- ^ restrict connections to HTTPS
		sessionTimeout :: Int,
			-- ^ in minutes
		portNumber :: Maybe Int,
			-- ^ useful to override for non-privileged testing
		userTemplate :: Text,
			-- starting template;  copied when a new user account be set up.
		userDir :: Text,
			-- directory containing user configurations.
		keysFile :: FilePath,
			-- ^ AES keys
		databaseUser :: Text,
			-- ^ default account with which to open database source tables.
		appRoot :: Text,
			-- ^ needed for identification emails
		howAuthorised :: AuthoriStyle.Style,
			-- ^ not sure and makes no sense now
		userConfig :: UserConfig
	}


runFilteredLoggingT :: MonadIO m => JRState -> LoggingT m a -> m a
runFilteredLoggingT site = LogFilter.runFilteredLoggingT (logLevel site)
