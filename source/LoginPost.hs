{-|
Description: Post-login processing:  load account name and credentials into session
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module LoginPost (getLoginR, getLoginPostR) where


import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation (Handler)
import qualified Data.Text as DT (Text, unpack, pack, concat)
import qualified Data.Map as DM (insert)
import qualified ConfigParse (content, Logged(..), UserSchema)
import qualified FailureMessage (page)
import qualified Logging
import qualified Data.Maybe as DMy (fromMaybe)
import qualified JRState (JRState(..))
import qualified Text.XML as XML (def, readFile)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import ReviewGet (getHomeR)


getLoginPostR :: Foundation.Handler YC.Html
getLoginPostR = YC.lookupSession YA.credsKey >>= maybe getHomeR getLoginR


getLoginR :: DT.Text -> Foundation.Handler YC.Html
getLoginR acctName = YC.getYesod >>= pong acctName


pong :: DT.Text -> JRState.JRState -> Foundation.Handler YC.Html

pong acctName site = userConfiguration >>= zumba where
	userConfiguration = YC.liftIO $ XML.readFile XML.def (DT.unpack contentName)
	contentName = DT.concat [JRState.userDir site, acctName, DT.pack ".cfg"]
	zumba zee = either FailureMessage.page (digest acctName site) (ConfigParse.content contentName (JRState.debugging site) zee)


digest :: DT.Text -> JRState.JRState -> ConfigParse.Logged ConfigParse.UserSchema -> Foundation.Handler YC.Html

digest acctName site (ConfigParse.Logged warnings info userSchema) =
	mapM_ Logging.logWarn warnings
		>> mapM_ Logging.logInfo (DMy.fromMaybe [] info)
		>> (YC.liftIO $ atomically $ modifyTVar' (JRState.userConfig site) (DM.insert acctName userSchema))
		>> getHomeR
