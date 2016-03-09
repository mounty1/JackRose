{-|
Description: Yesod 'master' data
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

This module is special inasmuch as it seems to be necessary, in order to fit
with the TH magic, to avoid module namespaces.  Therefore all symbols are
exported (even selective exporting doesn't work) and the module is imported
without @qualified@.
-}


{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Foundation where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Yesod.Auth.Account as YAA
import qualified Authorisation (User, SqlBackend, persistAction)
import qualified RouteData
import qualified EmailVerification
import qualified Data.Text as DT (concat)
import Data.Text (Text)
import JRState (JRState(..))


YC.mkYesodData "JRState" RouteData.routeData


instance YA.YesodAuth JRState where
	type AuthId JRState = YAA.Username
	getAuthId = return . Just . YA.credsIdent
	loginDest _ = LoginPostR
	logoutDest _ = AuthR YA.LoginR
	authPlugins _ = [YAA.accountPlugin]
	authHttpManager _ = error "No manager needed"
	onLogin = return ()
	maybeAuthId = YC.lookupSession YA.credsKey


instance YC.Yesod JRState where
	makeSessionBackend site =
		(if secureOnly site then YC.sslOnlySessions else id) $ fmap Just $ YC.defaultClientSessionBackend (sessionTimeout site) (keysFile site)
	yesodMiddleware handler = YC.getYesod >>= ourMiddleWare where
		ourMiddleWare site =
			(if secureOnly site then YC.sslOnlyMiddleware (sessionTimeout site) else YC.defaultYesodMiddleware) handler


instance Y.YesodPersist JRState where
	type YesodPersistBackend JRState = Authorisation.SqlBackend
	runDB action = YC.getYesod >>= (\site -> Authorisation.persistAction action (tablesFile site))


instance YC.RenderMessage JRState Y.FormMessage where
	renderMessage _ _ = Y.defaultFormMessage


instance YAA.YesodAuthAccount (YAA.AccountPersistDB JRState Authorisation.User) JRState where
	runAccountDB = YAA.runAccountPersistDB


emailEnaction :: (YC.MonadHandler m, YC.HandlerSite m ~ JRState) => (t -> t1 -> Text -> m b) -> t -> t1 -> Text -> m b
emailEnaction action uname email url = YC.getYesod >>= enact where
	enact site = action uname email fullURL where
		fullURL = DT.concat [appRoot site, url]


instance YAA.AccountSendEmail JRState where
	sendVerifyEmail uname email url = emailEnaction EmailVerification.newAccountEmail uname email url
	sendNewPasswordEmail uname email url = emailEnaction EmailVerification.resetAccountEmail uname email url
