{-|
Description: Take post of user score
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module ReviewPost (postHomeR) where


import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation
import qualified Data.Text as DT (Text)
import DespatchButtons (despatch)


-- | user has pressed a button; go on from there.
postHomeR :: Foundation.Handler YC.Html
postHomeR = YA.requireAuthId >> despatch Foundation.HomeR routeTable >>= YC.redirect


routeTable :: [( DT.Text, DT.Text -> Foundation.Destination)]
routeTable = [
		( "stats", const Foundation.HomeR ),
		( "OK", const Foundation.ScoreR ),
		( "logout", const $ Foundation.AuthR YA.LogoutR )
	]
