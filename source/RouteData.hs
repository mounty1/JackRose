{-# LANGUAGE QuasiQuotes #-}


module RouteData (routeData) where


import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA ()
import qualified Yesod.Routes.TH.Types as YT


routeData :: [YT.ResourceTree String]
routeData = [YC.parseRoutes|
	/ HomeR GET POST
	/auth AuthR YA.Auth YA.getAuth
	|]
