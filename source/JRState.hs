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
import Control.Monad.Logger (LoggingT, filterLogger, LogLevel(LevelDebug), runStdoutLoggingT)
import Control.Monad.IO.Class (MonadIO)


type UserConfig = TVar (Map Text UserSchema)


-- | The foundation object
data JRState = JRState {
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
		logLevel :: LogLevel,
			-- ^ detail of diagnostics
		howAuthorised :: AuthoriStyle.Style,
			-- ^ not sure and makes no sense now
		userConfig :: UserConfig
	}

debugging :: JRState -> Bool
debugging site = LevelDebug == logLevel site


runFilteredLoggingT :: MonadIO m => JRState -> LoggingT m a -> m a
runFilteredLoggingT site = runStdoutLoggingT . filterLogger (\_ -> (<=) (logLevel site))
-- runFilteredLoggingT _ = runStdoutLoggingT . filterLogger (\_ _ -> False)
