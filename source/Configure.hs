{-|
Description: Change foundation object with data from Configuration file
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Configure (siteObject) where


import qualified Data.Text as DT (pack, empty, Text, unpack, concat)
import Data.List ((\\))
import qualified CommandArgs (CmdLineArgs(..))
import qualified Data.ConfigFile as DC
import qualified Data.Maybe as DMy
import qualified AuthoriStyle (Style(..))
import qualified JRState (JRState(..), UserConfig)
import qualified Data.Map as DM (empty)
import Control.Concurrent.STM (newTVar)
import Control.Monad.STM (atomically)
import qualified Database.Persist.Sqlite as PerstQ (createSqlitePool)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, monadLoggerLog, logInfoN, logWarnN, logErrorN)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Logger (runStderrLoggingT)
import System.Log.FastLogger (toLogStr, fromLogStr)
import Data.Text.IO (hPutStrLn)
import System.IO (stderr)
import Data.Text.Encoding (decodeUtf8)


siteObject :: CommandArgs.CmdLineArgs -> IO JRState.JRState
siteObject argsMap = runStderrLoggingT $ liftIO $ ((atomically $ newTVar DM.empty) >>= configurationData) where

	-- feed the user data STM object into the reading of the configuration file.
	configurationData :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, m ~ IO) => JRState.UserConfig -> m JRState.JRState
	configurationData sharedData =
		-- if the configuration file name be specified, it must be readable, but the default file need not exist.
		(if conFallback then flip catchError warnBadConfigFile else id) (DC.readfile initConfig configName) >>=
			either (liftIO . return . error . errorInConfig) (assembleConf sharedData)
	warnBadConfigFile err = logWarnN (DT.pack $ show err) >> (liftIO $ return $ Right initConfig)

	-- feed the user data STM, the configuration file contents and now the database connection pool, into the creation of the site object.
	assembleConf :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, m ~ IO) => JRState.UserConfig -> DC.ConfigParser -> m JRState.JRState
	assembleConf sharedData configuration = connPoolM >>= makeAppObject where

		-- accumulate up the fields of the site object, one by one.
		-- the list of configuration key values is of course static, but subject to maintenance.
		makeAppObject pool = spliceShared (almostAppObject pool) (DC.options configuration defaultSection)
		almostAppObject pool =
			extractConfItem authory AuthoriStyle.Email "trustedSite" $
				extractConfItem id (CommandArgs.debuggery argsMap) "debug" $
					extractConfTextItem DT.empty "appRoot" $
						extractConfItem id "jackrose.aes" "keysFile" $
							extractConfTextItem "users/" "userDir" $
								extractConfTextItem "default.cfg" "userTemplate" $
									extractConfItem Just Nothing "portNumber" $
										extractConfNumItem 120 "sessionMinutes" $
											extractConfItem id True "secureSession" $ (connLabels, JRState.JRState pool)
		-- create the site object and return it, logging information, warnings and errors as required.
		spliceShared (labels, fn) keysE = either munchErr munchKeys keysE >> (liftIO $ return $ site) where
			munchErr = logErrorN . DT.pack . show
			munchKeys keys = (if JRState.debugging site then (>>) (mapM_ (logInfoN . makeUnseenKeyMsg) (labels \\ keysInConfig)) else id) (mapM_ (logWarnN . makeUnknownKeyMsg) (keysInConfig \\ labels)) where
				keysInConfig = map DT.pack keys
			site = fn sharedData

		-- poor man's Writer function;  extract a value from the configuration data and accumulate its key
		extractConfItem :: DC.Get_C s => (s -> b) -> b -> DT.Text -> ([DT.Text], b -> a) -> ([DT.Text], a)
		extractConfItem convert fallback label (labels, fn) = (label : labels, fn $ either (const fallback) convert (DC.get configuration defaultSection (DT.unpack label)))
		extractConfTextItem = extractConfItem DT.pack
		extractConfNumItem = extractConfItem read
		-- no point in opening a SQLite database more than once.
		(connLabels, connPoolM) = extractConfNumItem 1 "poolSize" $ extractConfTextItem "jackrose.sqlite" "tablesFile" ([], PerstQ.createSqlitePool)
	makeUnknownKeyMsg key = DT.concat [ DT.pack configName, ": unknown key ", key ]
	makeUnseenKeyMsg key = DT.concat [ DT.pack configName, ": using default ", key ]
	-- If a configuration file is specified, it has to exist;  otherwise the default name is used, but may not exist.
	conFallback = DMy.isNothing (CommandArgs.configName argsMap)
	configName = DMy.fromMaybe "/etc/jackrose.conf" (CommandArgs.configName argsMap)


initConfig :: DC.ConfigParser
initConfig = DC.emptyCP{DC.optionxform=id}


errorInConfig :: Show a => (a, String) -> String
errorInConfig (errData, location) = location ++ "###" ++ show errData


authory :: Bool -> AuthoriStyle.Style
authory False = AuthoriStyle.Trust
authory True = AuthoriStyle.Email


-- TODO:  what does this all mean?  http://stackoverflow.com/questions/15448206/yesod-exitfailure-1-when-installing-scaffolded-app
instance MonadLogger IO where
	monadLoggerLog _ _ _ = (hPutStrLn stderr) . decodeUtf8 . fromLogStr . toLogStr


defaultSection :: String
defaultSection  = "DEFAULT"
