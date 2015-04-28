{-|
Description: Top-level routing;  exports just the instances in @mkYesodDispatch
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances, TypeFamilies, MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application () where


import qualified Yesod as Y
import qualified Yesod.Auth as YA
import qualified Review (review, score)
import qualified Data.Maybe as DM
import Foundation


Y.mkYesodDispatch "JRState" resourcesJRState


getHomeR :: JRHandlerT Y.Html
getHomeR = YA.maybeAuthId >>= DM.maybe loginPlease Review.review


postHomeR :: JRHandlerT Y.Html
postHomeR = YA.maybeAuthId >>= DM.maybe loginPlease Review.score


loginPlease :: JRHandlerT Y.Html
loginPlease = Y.redirect (AuthR YA.LoginR)
