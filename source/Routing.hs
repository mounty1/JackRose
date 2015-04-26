-- {-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, ViewPatterns #-}
-- {-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}

{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeFamilies, TypeSynonymInstances #-}
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

module Routing where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Yesod.Auth.Account as YAA
import qualified Authorisation (User, persistAction, SqlBackend)
import qualified Foundation (JRState(..))
import qualified EmailVerification ()
import qualified RouteData


type JarrState = Foundation.JRState


Y.mkYesodData "JarrState" RouteData.routeData


type JRoute = YC.Route JarrState


instance YA.YesodAuth JarrState where
	type AuthId JarrState = YAA.Username
	getAuthId = return . Just . YA.credsIdent
	loginDest _ = HomeR
	logoutDest _ = AuthR YA.LoginR
	authPlugins _ = [YAA.accountPlugin]
	authHttpManager _ = error "No manager needed"
	onLogin = return ()
	maybeAuthId = Y.lookupSession YA.credsKey


instance Y.Yesod JarrState where
	makeSessionBackend site =
		(if Foundation.secureOnly site then Y.sslOnlySessions else id) $ fmap Just $ Y.defaultClientSessionBackend (Foundation.sessionTimeout site) (Foundation.keysFile site)
	yesodMiddleware handler = Y.getYesod >>= ourMiddleWare where
		ourMiddleWare site =
			(if Foundation.secureOnly site then Y.sslOnlyMiddleware (Foundation.sessionTimeout site) else Y.defaultYesodMiddleware) handler


type JRHandlerT wot = Y.HandlerT JarrState IO wot


instance Y.YesodPersist JarrState where
	type YesodPersistBackend JarrState = Authorisation.SqlBackend
	runDB action = Y.getYesod >>= Authorisation.persistAction action


-- this needs Y.Yesod JRState instance
instance YAA.YesodAuthAccount (YAA.AccountPersistDB JarrState Authorisation.User) JarrState where
	runAccountDB = YAA.runAccountPersistDB


instance Y.RenderMessage JarrState Y.FormMessage where
	renderMessage _ _ = Y.defaultFormMessage
