{-# LANGUAGE TemplateHaskell, OverloadedStrings, TypeSynonymInstances #-}

{-|
Module      : Main
Description : Entry point for JackRose
Copyright   : (c) Michael Mounteney, 2015
License     : BSD 3 clause
Maintainer  : jackrose@landcroft.com
Stability   : experimental
Portability : undefined

Jackrose is a @spaced repetition web service@.
-}

module Main where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Authorisation (upgradeDB)
import qualified Foundation (JRState(..), siteObject)
import qualified EmailVerification ()
import qualified CommandArgs (args)
import Routing
import qualified RouteData (routeData)
import qualified Review (review, score)
import qualified Data.Maybe as DM


Y.mkYesodDispatch "JarrState" RouteData.routeData


getHomeR :: Routing.JRHandlerT Y.Html
getHomeR = YA.maybeAuthId >>= DM.maybe loginPlease Review.review


postHomeR :: Routing.JRHandlerT Y.Html
postHomeR = YA.maybeAuthId >>= DM.maybe loginPlease Review.score


loginPlease :: Routing.JRHandlerT Y.Html
loginPlease = YC.redirect (Routing.AuthR YA.LoginR)


-- | Start here
main :: IO ()
-- | Turn the command line arguments into a map of option letters to arguments,
-- | then use that to inform the construction of the foundation siteObject,
-- | then check that the authorisation table is in the current format,
-- | and finally hand over to Warp, to launch the service.
main = CommandArgs.args >>= Foundation.siteObject >>= letsGo


letsGo :: Foundation.JRState -> IO ()
letsGo site = Authorisation.upgradeDB site >> Y.warp 3000 site
