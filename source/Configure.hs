{-|
Description: Change foundation object with data from Configuration file
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}


module Configure (siteObject, portTCP) where


import qualified Data.Text as DT (pack, empty)
import qualified CommandArgs (CmdLineArgs(..))
import qualified Data.ConfigFile as DC
import qualified Control.Monad.Except as CME
import qualified Data.List as DL (lookup)
import qualified Data.Maybe as DM
import qualified AuthoriStyle (Style(..))
import qualified JRState (JRState(..), UserConfig)
import qualified Data.Map as DM (empty)
import Control.Concurrent.STM (newTVar)
import Control.Monad.STM (atomically)


siteObject :: CommandArgs.CmdLineArgs -> IO JRState.JRState
siteObject argsMap = (atomically $ newTVar DM.empty) >>= newConfiguration where
	newConfiguration confMap = configToSite configFileName (baseSiteObject confMap) {JRState.debugging = CommandArgs.debuggery argsMap}
	configFileName = DM.fromMaybe defaultConfigFileName (CommandArgs.configName argsMap)


configToSite :: String -> JRState.JRState -> IO JRState.JRState
configToSite configName site = (CME.runExceptT $ pipe configName site) >>= either (estate site) return


portTCP :: JRState.JRState -> Int
portTCP site = DM.fromMaybe (if JRState.secureOnly site then 443 else 80) (JRState.portNumber site)


estate :: Show t => JRState.JRState -> t -> IO JRState.JRState
estate site err = (putStrLn $ "<<" ++ show err ++ ">>") >> return site


pipe :: (CME.MonadError DC.CPError m, CME.MonadIO m) => FilePath -> JRState.JRState -> m JRState.JRState
pipe configName site =
	(CME.join $ CME.liftIO $ DC.readfile DC.emptyCP{DC.optionxform=id} configName) >>= foldInCfg site


foldInCfg :: Monad m => JRState.JRState -> DC.ConfigParser -> m JRState.JRState
foldInCfg site configuration =
	return $ foldl mergeIn site seckeys where
		mergeIn ss0 key = splice ss0 (DL.lookup key siteAlterMap) where
			splice _ Nothing = error $ "invalid key " ++ key ++ " in configuration file"
			splice ss (Just fn) = splice' ss fn
			splice' ss (AB fn) = fn ss arg where (Right arg) = DC.get configuration defaultSection key
			splice' ss (AI fn) = fn ss arg where (Right arg) = DC.get configuration defaultSection key
			splice' ss (AS fn) = fn ss arg where (Right arg) = DC.get configuration defaultSection key
		(Right seckeys) = DC.options configuration defaultSection


type SiteAlterFn a = JRState.JRState -> a -> JRState.JRState


data SiteAlterVector = AB (SiteAlterFn Bool)
	| AI (SiteAlterFn Int)
	| AS (SiteAlterFn String)


siteAlterMap :: [(DC.OptionSpec, SiteAlterVector)]
siteAlterMap = [
	("secureSession", AB (\site t -> site{JRState.secureOnly = t})),
	("sessionMinutes", AI (\site t -> site{JRState.sessionTimeout = t})),
	("portNumber", AI (\site t -> site{JRState.portNumber = Just t})),
	("tablesFile", AS (\site t -> site{JRState.tablesFile = DT.pack t})),
	("userTemplate", AS (\site t -> site{JRState.userTemplate = DT.pack t})),
	("userDir", AS (\site t -> site{JRState.userDir = DT.pack t})),  -- ^ TODO check it has a final /
	("appRoot", AS (\site t -> site{JRState.appRoot = DT.pack t})),
	("trustedSite", AB (\site t -> if t then site{JRState.howAuthorised = AuthoriStyle.Trust} else site)),
	("keysFile", AS (\site t -> site{JRState.keysFile = t}))
	]


defaultSection :: String
defaultSection  = "DEFAULT"


baseSiteObject :: JRState.UserConfig -> JRState.JRState
baseSiteObject conf = JRState.JRState True 120 Nothing "jackrose.sqlite" "default.cfg" "users/" "jackrose.aes" DT.empty False AuthoriStyle.Email conf


defaultConfigFileName :: String
defaultConfigFileName = "/etc/jackrose.conf"
