{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Routing
Description : Entry point for JackRose
Copyright   : (c) Michael Mounteney, 2015
License     : BSD 3 clause
Maintainer  : jackrose@landcroft.com
Stability   : experimental
Portability : undefined

This module is special inasmuch as it seems to be necessary, in order to fit
with the TH magic, to avoid module namespaces.  Therefore all symbols are
exported (even selecting exporting doesn't work) and the module is imported
with 'qualified'.
-}

module Foundation where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Yesod.Auth.Account as YAA
import qualified Authorisation (User, SqlBackend, persistAction)
import qualified RouteData
import qualified EmailVerification
import qualified Pervasive (TextItem)


-- | The foundation object
data JRState = JRState {
		secureOnly :: Bool,  -- ^ restrict connections to HTTPS
		sessionTimeout :: Int,  -- ^ session time-out in minutes
		authTable :: Pervasive.TextItem, -- ^ name of SQLite3 file of authorised users
		keysFile :: FilePath,  -- ^ AES keys
		debugging :: Bool   -- ^ output more information
	}


YC.mkYesodData "JRState" RouteData.routeData


type JRHandlerT wot = YC.HandlerT JRState IO wot


goHome, loginPlease :: JRHandlerT Y.Html
goHome = YC.redirect HomeR
loginPlease = YC.redirect (AuthR YA.LoginR)


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
	makeSessionBackend site =
		(if secureOnly site then YC.sslOnlySessions else id) $ fmap Just $ YC.defaultClientSessionBackend (sessionTimeout site) (keysFile site)
	yesodMiddleware handler = YC.getYesod >>= ourMiddleWare where
		ourMiddleWare site =
			(if secureOnly site then YC.sslOnlyMiddleware (sessionTimeout site) else YC.defaultYesodMiddleware) handler


instance Y.YesodPersist JRState where
	type YesodPersistBackend JRState = Authorisation.SqlBackend
	runDB action = YC.getYesod >>= (\site -> Authorisation.persistAction action (authTable site))


instance YC.RenderMessage JRState Y.FormMessage where
	renderMessage _ _ = Y.defaultFormMessage


-- this needs YC.Yesod JRState instance
instance YAA.YesodAuthAccount (YAA.AccountPersistDB JRState Authorisation.User) JRState where
	runAccountDB = YAA.runAccountPersistDB


instance YAA.AccountSendEmail JRState where
	sendVerifyEmail uname email url = error $ "FUCK THIS SHIT" -- {- YC.getYesod >>= -} EmailVerification.newAccountEmail uname email url
	sendNewPasswordEmail uname email url = error $ "FUCK THIS SHIT" -- {- YC.getYesod >>= -} EmailVerification.resetAccountEmail uname email url
