{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}

{-|
Module      : Main
Description : Entry point for JackRose
Copyright   : (c) Michael Mounteney, 2015
License     : BSD 3 caluse
Maintainer  : jackrose@landcroft.com
Stability   : experimental
Portability : undefined

Jackrose is a @spaced repetition web service@.
-}

module Main where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Yesod.Auth.Account as YAA
import qualified Authorisation (User, persistAction, SqlBackend, upgradeDB)
import qualified Foundation (JRState(..), siteObject)
import qualified Pervasive (TextItem)
import qualified EmailVerification ()
import qualified CommandArgs (args)
import qualified Review (review)


type JarrState = Foundation.JRState

Y.mkYesod "JarrState" [YC.parseRoutes|
	/ HomeR GET
	/auth AuthR YA.Auth YA.getAuth
	|]


type JRoute = Y.Route Foundation.JRState


instance YA.YesodAuth Foundation.JRState where
	type AuthId Foundation.JRState = YAA.Username
	getAuthId = return . Just . YA.credsIdent
	loginDest _ = HomeR
	logoutDest _ = AuthR YA.LoginR
	authPlugins _ = [YAA.accountPlugin]
	authHttpManager _ = error "No manager needed"
	onLogin = return ()
	maybeAuthId = Y.lookupSession YA.credsKey


instance Y.Yesod Foundation.JRState where
	makeSessionBackend site =
		(if Foundation.secureOnly site then Y.sslOnlySessions else id) $ fmap Just $ Y.defaultClientSessionBackend (Foundation.sessionTimeout site) (Foundation.keysFile site)
	yesodMiddleware handler = Y.getYesod >>= ourMiddleWare handler


type JRHandlerT wot = Y.HandlerT Foundation.JRState IO wot


ourMiddleWare :: JRHandlerT res -> Foundation.JRState -> JRHandlerT res
ourMiddleWare handler site =
		(if Foundation.secureOnly site then Y.sslOnlyMiddleware (Foundation.sessionTimeout site) else Y.defaultYesodMiddleware) handler


instance Y.YesodPersist Foundation.JRState where
	type YesodPersistBackend Foundation.JRState = Authorisation.SqlBackend
	runDB action = Y.getYesod >>= Authorisation.persistAction action


-- this needs Y.Yesod JRState instance
instance YAA.YesodAuthAccount (YAA.AccountPersistDB Foundation.JRState Authorisation.User) Foundation.JRState where
	runAccountDB = YAA.runAccountPersistDB


instance Y.RenderMessage Foundation.JRState Y.FormMessage where
	renderMessage _ _ = Y.defaultFormMessage


getHomeR :: JRHandlerT Y.Html
getHomeR = YA.maybeAuthId >>= ensureAuthenticated


ensureAuthenticated :: Maybe Pervasive.TextItem -> JRHandlerT Y.Html
ensureAuthenticated Nothing = YC.redirect (AuthR YA.LoginR)
ensureAuthenticated (Just username) = Review.review username


-- | Start here
main :: IO ()
-- | Turn the command line arguments into a map of option letters to arguments,
-- | then use that to inform the construction of the foundation siteObject,
-- | then check that the authorisation table is in the current format,
-- -- | and finally hand over to Warp, to launch the service.
main = CommandArgs.args >>= Foundation.siteObject >>= letsGo


letsGo :: Foundation.JRState -> IO ()
letsGo site = Authorisation.upgradeDB site >> Y.warp 3000 site
