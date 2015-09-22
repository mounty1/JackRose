{-|
Description: TH argument;  isolated to contain the scope of LANGUAGE extensions.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses, QuasiQuotes #-}


module RouteData (routeData) where


import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA ()
import qualified Yesod.Routes.TH.Types as YT


-- | this is still a magic black box
routeData :: [YT.ResourceTree String]
routeData = [YC.parseRoutes|
	/ HomeR GET POST
	/auth AuthR YA.Auth YA.getAuth
	/account/#Text LoginR GET
	|]
