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
{-# OPTIONS_HADDOCK prune #-}


module Foundation where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Yesod.Auth.Account as YAA
import qualified Authorisation (User)
import qualified Database.Persist.Sql as PerstQ (SqlBackend, runSqlPool)
import qualified RouteData
import qualified EmailVerification
import Data.Text (Text)
import JRState (JRState(..))


YC.mkYesodData "JRState" RouteData.routeData


-- | This seems to be needed for older compilers;  ghc 8.0.2 can use 'Foundation.Route JRState' directly.
type Destination = YC.Route JRState


instance YA.YesodAuth JRState where
	type AuthId JRState = YAA.Username
	getAuthId = return . Just . YA.credsIdent
	loginDest _ = HomeR
	logoutDest _ = AuthR YA.LoginR
	authPlugins _ = [YAA.accountPlugin]
	authHttpManager _ = error "No manager needed"
	onLogin = return ()
	maybeAuthId = YC.lookupSession YA.credsKey


instance YC.Yesod JRState where
	authRoute _ = Just $ AuthR YA.LoginR
	makeSessionBackend site =
		(if secureOnly site then YC.sslOnlySessions else id) $ Just <$> YC.defaultClientSessionBackend (sessionTimeout site) (keysFile site)
	yesodMiddleware handler = YC.getYesod >>= ourMiddleWare where
		ourMiddleWare site =
			(if secureOnly site then YC.sslOnlyMiddleware (sessionTimeout site) else YC.defaultYesodMiddleware) handler


instance Y.YesodPersist JRState where
	type YesodPersistBackend JRState = PerstQ.SqlBackend
	runDB action = fmap tablesFile YC.getYesod >>= PerstQ.runSqlPool action


instance YC.RenderMessage JRState Y.FormMessage where
	renderMessage _ _ = Y.defaultFormMessage


instance YAA.YesodAuthAccount (YAA.AccountPersistDB JRState Authorisation.User) JRState where
	runAccountDB = YAA.runAccountPersistDB


instance YAA.AccountSendEmail JRState where
	sendVerifyEmail = EmailVerification.newAccountEmail
	sendNewPasswordEmail = EmailVerification.resetAccountEmail
