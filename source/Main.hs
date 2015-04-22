{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}

module Main where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Yesod.Auth.Account as YAA
import qualified Authorisation (User, persistAction, SqlBackend, upgradeDB)
import qualified Foundation (JRState(..), siteObject)
import qualified Pervasive (TextItem, fromByteS, length)
import qualified EmailVerification ()


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
	makeSessionBackend (Foundation.JRState secureOnly sessionMins _ keysFileName) =
		(if secureOnly then Y.sslOnlySessions else id) $ fmap Just $ Y.defaultClientSessionBackend sessionMins keysFileName
	yesodMiddleware handler = Y.getYesod >>= ourMiddleWare handler


type JRHandlerT wot = Y.HandlerT Foundation.JRState IO wot


ourMiddleWare :: JRHandlerT res -> Foundation.JRState -> JRHandlerT res
ourMiddleWare handler (Foundation.JRState isSecure sessionMins _ _) =
		(if isSecure then Y.sslOnlyMiddleware sessionMins else Y.defaultYesodMiddleware) handler


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
ensureAuthenticated (Just username) = Y.defaultLayout $(Y.whamletFile "loggedin.hamlet")


main :: IO ()
main = Foundation.siteObject >>= letsGo

letsGo :: Foundation.JRState -> IO ()
letsGo site = Authorisation.upgradeDB site >> Y.warp 3000 site
