{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Foundation (JRState(..), siteObject) where


import qualified Pervasive (TextItem, pack)
import qualified CommandArgs (CmdLineArgs(..))
import qualified Data.ConfigFile as DC
import qualified Control.Monad.Except as CME
import qualified Data.List as DL (lookup)
import qualified Data.Maybe as DM


-- | The foundation object
data JRState = JRState {
		secureOnly :: Bool,  -- ^ restrict connections to HTTPS
		sessionTimeout :: Int,  -- ^ session time-out in minutes
		authTable :: Pervasive.TextItem, -- ^ name of SQLite3 file of authorised users
		keysFile :: FilePath,  -- ^ AES keys
		debugging :: Bool   -- ^ output more information
	}


siteObject :: CommandArgs.CmdLineArgs -> IO JRState
siteObject argsMap = configToSite configFileName baseSiteObject{debugging = CommandArgs.debuggery argsMap} where
	configFileName = DM.fromMaybe defaultConfigFileName (CommandArgs.configName argsMap)


configToSite :: String -> JRState -> IO JRState
configToSite configName site = (CME.runExceptT $ pipe configName site) >>= estate


estate :: Show t => Either t JRState -> IO JRState
estate (Right state) = return state
estate (Left err) = (putStrLn $ "<<" ++ show err ++ ">>") >> return baseSiteObject


pipe :: (CME.MonadError DC.CPError m, CME.MonadIO m) => FilePath -> JRState -> m JRState
pipe configName site =
	(CME.join $ CME.liftIO $ DC.readfile DC.emptyCP{DC.optionxform=id} configName) >>= foldInCfg site


foldInCfg :: Monad m => JRState -> DC.ConfigParser -> m JRState
foldInCfg site configuration =
	return $ foldl mergeIn site seckeys where
		mergeIn ss0 key = splice ss0 (DL.lookup key siteAlterMap) where
			splice _ Nothing = error $ "invalid key " ++ key ++ " in configuration file"
			splice ss (Just fn) = splice' ss fn
			splice' ss (AB fn) = fn ss arg where (Right arg) = DC.get configuration defaultSection key
			splice' ss (AI fn) = fn ss arg where (Right arg) = DC.get configuration defaultSection key
			splice' ss (AS fn) = fn ss arg where (Right arg) = DC.get configuration defaultSection key
		(Right seckeys) = DC.options configuration defaultSection


type SiteAlterFn a = JRState -> a -> JRState


data SiteAlterVector = AB (SiteAlterFn Bool)
	| AI (SiteAlterFn Int)
	| AS (SiteAlterFn String)


siteAlterMap :: [(DC.OptionSpec, SiteAlterVector)]
siteAlterMap = [
	("secureSession", AB (\site t -> site{secureOnly = t})),
	("sessionMinutes", AI (\site t -> site{sessionTimeout = t})),
	("userDatabase", AS (\site t -> site{authTable = Pervasive.pack t})),
	("keysFile", AS (\site t -> site{keysFile = t}))
	]


defaultSection :: String
defaultSection  = "DEFAULT"


baseSiteObject :: JRState
baseSiteObject = JRState False 120 "users.sqlite" "jackrose-keys.aes" False


defaultConfigFileName :: String
defaultConfigFileName = "/etc/jackrose.cfg"
