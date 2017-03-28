{-|
Description: Display a message.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module Notice (getNoticeR, postNoticeR) where


import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation
import Data.Text (Text)
import qualified PresentHTML as PH
import DespatchButtons (despatch)


-- | Display a simple message.
getNoticeR :: Text -> Foundation.Handler YC.Html
getNoticeR = PH.toHTMLdoc . PH.documentHTMLNotice


-- | Respond to button-press from displayed message.
postNoticeR :: Text -> Foundation.Handler YC.Html
postNoticeR _ = YA.requireAuthId >> despatch Foundation.HomeR routeTable >>= YC.redirect


routeTable :: [( Text, Text -> Foundation.Destination)]
routeTable = [
		( "stats", const Foundation.HomeR ),
		( "OK", const Foundation.HomeR ),
		( "logout", const $ Foundation.AuthR YA.LogoutR )
	]
