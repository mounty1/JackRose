{-|
Description: Yesod Foundation type
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module JRState where


import qualified AuthoriStyle (Style)
import Data.Text (Text)
import ConfigParse (UserSchema)
import Data.Map (Map)
import Control.Concurrent.STM (TVar)


type UserConfig = TVar (Map Text UserSchema)


-- | The foundation object
data JRState = JRState {
		secureOnly :: Bool,
			-- ^ restrict connections to HTTPS
		sessionTimeout :: Int,
			-- ^ in minutes
		portNumber :: Maybe Int,
			-- ^ useful to override for non-privileged testing
		tablesFile :: Text,
			-- ^ name of file containing SQLite3 tables
		userTemplate :: Text,
			-- starting template;  copied when a new user account be set up.
		userDir :: Text,
			-- directory containing user configurations.
		keysFile :: FilePath,
			-- ^ AES keys
		appRoot :: Text,
			-- ^ needed for identification emails
		debugging :: Bool,
			-- ^ output more information
		howAuthorised :: AuthoriStyle.Style,
			-- ^ not sure and makes no sense now
		userConfig :: UserConfig
	}
