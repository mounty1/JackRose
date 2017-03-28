{-|
Description: Construct foundation object with data from Configuration file.
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module SiteConfigFromFile (siteObject) where


import qualified Data.Text as DT (pack, empty, Text, unpack, concat, append, toLower)
import Data.List ((\\))
import qualified CommandArgs (CmdLineArgs(..))
import qualified Data.ConfigFile as DC
import qualified Data.Maybe as DMy
import qualified AuthoriStyle (Style(..))
import qualified JRState (JRState(..), PostgresConnPool, DataSchemes, UserConfig)
import qualified Data.Map as DM (empty)
import Database.Persist.Sql (ConnectionPool)
import Control.Concurrent.STM (newTVar, TVar)
import Control.Monad.STM (atomically)
import Control.Monad.Logger (LogLevel(..), logDebugNS, logWarnNS, logErrorNS)
import Control.Monad.Error.Class (catchError)
import LogFilter (runFilteredLoggingT)
import qualified Branding (innerName)
import PersistOpenGeneral (createSqlAnyPool)


nameSql :: DT.Text
nameSql = DT.append Branding.innerName ".sqlite"


-- | Called once when the application starts;  it takes the command line parameters object and
-- constructs the Yesod foundation object, logging any warnings, errors and information.
siteObject :: CommandArgs.CmdLineArgs -> IO JRState.JRState
siteObject argsMap = atomically (newTVar DM.empty) >>= \r -> atomically (newTVar DM.empty) >>= \s -> atomically (newTVar DM.empty) >>= configurationData r s where

	-- feed the user data STM object into the reading of the configuration file.
	configurationData :: TVar JRState.PostgresConnPool -> TVar JRState.DataSchemes -> TVar JRState.UserConfig -> IO JRState.JRState
	configurationData connections dataSchemes sharedData =
		-- if the configuration file name be specified, it must be readable, but the default file need not exist.
		(if conFallback then flip catchError warnBadConfigFile else id) (DC.readfile initConfig configName) >>=
			either (return . error . errorInConfig) (assembleConf connections dataSchemes sharedData)

	-- we can't use the configured logging level because the configuration file can't be read.
	warnBadConfigFile :: Show s => s -> IO (Either a DC.ConfigParser)
	warnBadConfigFile err = runFilteredLoggingT fallbackDebugLevel (logWarnNS configLogName (DT.pack $ show err)) >> (return $ Right initConfig)

	-- feed the user data STM, the configuration file contents and now the database connection pool, into the creation of the site object.
	assembleConf :: TVar JRState.PostgresConnPool -> TVar JRState.DataSchemes -> TVar JRState.UserConfig -> DC.ConfigParser -> IO JRState.JRState
	assembleConf connections dataSchemes sharedData configuration = runFilteredLoggingT verbosity connPoolM >>= makeAppObject where

		-- accumulate up the fields of the site object, one by one.
		-- the list of configuration key values is of course static, but subject to maintenance.
		-- Don't try to read anything in class Read because the parser expects quotation marks around it
		makeAppObject :: ConnectionPool -> IO JRState.JRState
		makeAppObject pool = spliceShared (almostAppObject pool) (DC.options configuration defaultSection)
		almostAppObject pool =
			extractConfItem authory AuthoriStyle.Email "trustedSite" $
				extractConfTextItem DT.empty "appRoot" $
					extractConfTextItem Branding.innerName "dbuser" $
						extractConfItem id (DT.unpack Branding.innerName ++ ".aes") "keysFile" $
							extractConfItem Just Nothing "portNumber" $
								extractConfNumItem 120 "sessionMinutes" $
									extractConfItem id False "shuffle" $
										extractConfItem id True "secureSession" $ (connLabels, JRState.JRState verbosity pool)
		-- create the site object and return it, logging information, warnings and errors as required.
		spliceShared :: Show l => ([DT.Text], TVar JRState.PostgresConnPool -> TVar JRState.DataSchemes -> TVar JRState.UserConfig -> JRState.JRState) -> Either l [String] -> IO JRState.JRState
		spliceShared (labels, fn) keysE = runFilteredLoggingT verbosity (either munchErr munchKeys keysE) >> return site where
			munchErr = logErrorNS configLogName . DT.pack . show
			munchKeys keys = mapM_ (logDebugNS configLogName . makeUnseenKeyMsg) (labels \\ keysInConfig) >> mapM_ (logWarnNS configLogName . makeUnknownKeyMsg) (keysInConfig \\ labels) where
				keysInConfig = map DT.pack keys
			site = fn connections dataSchemes sharedData

		-- poor man's Writer function;  extract a value from the configuration data and accumulate its key.
		extractConfItem :: DC.Get_C s => (s -> b) -> b -> DT.Text -> ([DT.Text], b -> a) -> ([DT.Text], a)
		extractConfItem convert fallback label (labels, fn) = (label : labels, fn $ either (const fallback) convert (DC.get configuration defaultSection (DT.unpack label)))
		extractConfTextItem = extractConfItem DT.pack
		extractConfNumItem = extractConfItem read

		-- no point in opening a SQLite database more than once.
		(connLabels, connPoolM) = extractConfNumItem 1 "poolSize" $ extractConfTextItem nameSql "tablesFile" (verbLabel, createSqlAnyPool)

		-- we need verbosity early to control logging level
		(verbLabel, verbosity) = extractConfItem logValue fallbackDebugLevel "verbosity" ([], id)

	fallbackDebugLevel = if CommandArgs.debuggery argsMap then LevelDebug else defaultLogLevel

	makeUnknownKeyMsg key = DT.concat [ "unknown key ", key ]
	makeUnseenKeyMsg key = DT.concat [ "using default ", key ]
	-- If a configuration file is specified, it has to exist;  otherwise the default name is used, but may not exist.
	conFallback = DMy.isNothing (CommandArgs.configName argsMap)
	configName = DMy.fromMaybe ( "/etc/" ++ DT.unpack Branding.innerName ++ ".conf" ) (CommandArgs.configName argsMap)
	configLogName = DT.pack configName


logValue :: String -> LogLevel
logValue word = DMy.fromMaybe defaultLogLevel (lookup (DT.toLower $ DT.pack word) debugLevels)


debugLevels :: [(DT.Text, LogLevel)]
debugLevels = [ ("verbose", LevelDebug), ("normal", LevelInfo), ("terse", LevelWarn) ]


defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo


initConfig :: DC.ConfigParser
initConfig = DC.emptyCP{DC.optionxform=id}


errorInConfig :: Show a => (a, String) -> String
errorInConfig (errData, location) = location ++ "###" ++ show errData


authory :: Bool -> AuthoriStyle.Style
authory False = AuthoriStyle.Trust
authory True = AuthoriStyle.Email


defaultSection :: String
defaultSection = "DEFAULT"
