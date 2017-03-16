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


import qualified Yesod as Y
import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation
import Data.Text (Text)
import qualified PresentHTML as PH
import qualified Branding (visibleName)


-- | Display a simple message.
getNoticeR :: Text -> Foundation.Handler YC.Html
getNoticeR = PH.toHTMLdoc . PH.documentHTML Nothing Branding.visibleName


-- | Respond to button-press from displayed message.
postNoticeR :: Text -> Foundation.Handler YC.Html
postNoticeR _ = YA.requireAuthId >>= proceed


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
-- | user has pressed a button; go on from there.
proceed :: Text -> Foundation.Handler YC.Html
proceed _ = (Y.runInputPost $ triple <$> Y.iopt Y.textField "stats" <*> Y.iopt Y.textField "OK" <*> Y.iopt Y.textField "logout") >>= YC.redirect . enaction


type OpText = Maybe Text


triple :: OpText -> OpText -> OpText -> (OpText, OpText, OpText)
triple one two three = (one, two, three)


enaction :: (OpText, OpText, OpText) -> Foundation.Destination
-- "stats" button pressed;  go to upload/download screen (not yet written)
enaction (Just _, _, _) = Foundation.HomeR
-- OK button pressed;  work out which one and score this item
enaction (Nothing, Just _, _) = Foundation.HomeR
-- "logout" button pressed;  so do it.
enaction (Nothing, Nothing, Just _) = Foundation.AuthR YA.LogoutR
-- "this should never happen";  not sure what to do here.
enaction (Nothing, Nothing, Nothing) = Foundation.HomeR
