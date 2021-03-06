{-|
Description: Route table;  isolated to contain the scope of LANGUAGE extensions.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}


module RouteData (routeData) where


import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA ()
import qualified Yesod.Routes.TH.Types as YT


-- | Routing table.
routeData :: [YT.ResourceTree String]
routeData = [YC.parseRoutes|
  / HomeR GET POST
  /favicon.ico IconR GET
  /score ScoreR GET POST
  /notice/#Text NoticeR GET POST
  /review/#Text ReviewR GET
  /auth AuthR YA.Auth YA.getAuth
  |]
