{-|
Description: Post-login processing:  load account name and credentials into session
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module LoginPost (getLoginR) where


import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation (Handler)
import qualified Data.Text as DT (Text, unpack, pack, concat)
import qualified Data.Map as DM (insert)
import qualified AuthoriStyle (Style(..))
import qualified ConfigParse (content, Logged(..), SchemaParsing)
import qualified FailureMessage (page)
import qualified Logging
import qualified Data.Maybe as DMy (fromMaybe)
import qualified JRState (JRState(..))
import qualified Text.XML as XML (def, readFile)
import Control.Concurrent.STM.TVar (modifyTVar')
import GoHome (goHome)


getLoginR :: DT.Text -> Foundation.Handler YC.Html
getLoginR acctName = YC.getYesod >>= pong acctName


pong :: DT.Text -> JRState.JRState -> Foundation.Handler YC.Html

pong acctName site = userConfiguration >>= digest acctName site . ConfigParse.content contentName (JRState.debugging site) where
	userConfiguration = YC.liftIO $ XML.readFile XML.def (DT.unpack contentName)
	contentName = DT.concat [JRState.userDir site, acctName, DT.pack ".cfg"]


digest :: DT.Text -> JRState.JRState -> ConfigParse.SchemaParsing -> Foundation.Handler YC.Html

digest _ _ (Left failReason) = FailureMessage.page failReason

digest acctName site (Right (ConfigParse.Logged warnings info userSchema)) =
	mapM_ Logging.logWarn warnings
		>> mapM_ Logging.logInfo (DMy.fromMaybe [] info)
		>> YC.liftIO (JRState.userConfig site >>= (return . flip modifyTVar' (DM.insert acctName userSchema)))
		>> setAcctIfTrusted acctName (JRState.howAuthorised site)


setAcctIfTrusted :: DT.Text -> AuthoriStyle.Style -> Foundation.Handler YC.Html
setAcctIfTrusted _ AuthoriStyle.Email = goHome
setAcctIfTrusted acctName AuthoriStyle.Trust = load acctName >> goHome


-- Given a logged-in user name, load its config data into the session
load :: YC.MonadHandler m => DT.Text -> m ()
load acctName = YC.setSession YA.credsKey acctName
