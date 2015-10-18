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
import qualified Foundation (JRState(..))
import qualified AuthoriStyle (Style(..))


siteObject :: CommandArgs.CmdLineArgs -> IO Foundation.JRState
siteObject argsMap = configToSite configFileName baseSiteObject{Foundation.debugging = CommandArgs.debuggery argsMap} where
	configFileName = DM.fromMaybe defaultConfigFileName (CommandArgs.configName argsMap)


configToSite :: String -> Foundation.JRState -> IO Foundation.JRState
configToSite configName site = (CME.runExceptT $ pipe configName site) >>= estate


portTCP :: Foundation.JRState -> Int
portTCP site = DM.fromMaybe (if Foundation.secureOnly site then 443 else 80) (Foundation.portNumber site)


estate :: Show t => Either t Foundation.JRState -> IO Foundation.JRState
estate (Right state) = return state
estate (Left err) = (putStrLn $ "<<" ++ show err ++ ">>") >> return baseSiteObject


pipe :: (CME.MonadError DC.CPError m, CME.MonadIO m) => FilePath -> Foundation.JRState -> m Foundation.JRState
pipe configName site =
	(CME.join $ CME.liftIO $ DC.readfile DC.emptyCP{DC.optionxform=id} configName) >>= foldInCfg site


foldInCfg :: Monad m => Foundation.JRState -> DC.ConfigParser -> m Foundation.JRState
foldInCfg site configuration =
	return $ foldl mergeIn site seckeys where
		mergeIn ss0 key = splice ss0 (DL.lookup key siteAlterMap) where
			splice _ Nothing = error $ "invalid key " ++ key ++ " in configuration file"
			splice ss (Just fn) = splice' ss fn
			splice' ss (AB fn) = fn ss arg where (Right arg) = DC.get configuration defaultSection key
			splice' ss (AI fn) = fn ss arg where (Right arg) = DC.get configuration defaultSection key
			splice' ss (AS fn) = fn ss arg where (Right arg) = DC.get configuration defaultSection key
		(Right seckeys) = DC.options configuration defaultSection


type SiteAlterFn a = Foundation.JRState -> a -> Foundation.JRState


data SiteAlterVector = AB (SiteAlterFn Bool)
	| AI (SiteAlterFn Int)
	| AS (SiteAlterFn String)


siteAlterMap :: [(DC.OptionSpec, SiteAlterVector)]
siteAlterMap = [
	("secureSession", AB (\site t -> site{Foundation.secureOnly = t})),
	("sessionMinutes", AI (\site t -> site{Foundation.sessionTimeout = t})),
	("portNumber", AI (\site t -> site{Foundation.portNumber = Just t})),
	("tablesFile", AS (\site t -> site{Foundation.tablesFile = DT.pack t})),
	("userTemplate", AS (\site t -> site{Foundation.userTemplate = DT.pack t})),
	("userDir", AS (\site t -> site{Foundation.userDir = DT.pack t})),  -- ^ TODO check it has a final /
	("appRoot", AS (\site t -> site{Foundation.appRoot = DT.pack t})),
	("trustedSite", AB (\site t -> if t then site{Foundation.howAuthorised = AuthoriStyle.Trust} else site)),
	("keysFile", AS (\site t -> site{Foundation.keysFile = t}))
	]


defaultSection :: String
defaultSection  = "DEFAULT"


baseSiteObject :: Foundation.JRState
baseSiteObject = Foundation.JRState True 120 Nothing "jackrose.sqlite" "default.cfg" "users/" "jackrose.aes" DT.empty False AuthoriStyle.Email


defaultConfigFileName :: String
defaultConfigFileName = "/etc/jackrose.conf"
