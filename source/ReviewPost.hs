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


import qualified Yesod as Y
import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation
import Data.Text (Text)


-- | user has scored their item so re-schedule it and move to the next.
postHomeR :: Foundation.Handler YC.Html
postHomeR = YA.requireAuthId >>= const reveal


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
-- | user has pressed a button; go on from there.
reveal :: Foundation.Handler YC.Html
reveal = (Y.runInputPost $ triple <$> Y.iopt Y.textField "stats" <*> Y.iopt Y.textField "OK" <*> Y.iopt Y.textField "logout") >>= YC.redirect . enaction


type OpText = Maybe Text


triple :: OpText -> OpText -> OpText -> (OpText, OpText, OpText)
triple one two three = (one, two, three)


enaction :: (OpText, OpText, OpText) -> Foundation.Destination
-- "stats" button pressed;  go to upload/download screen (not yet written)
enaction (Just _, _, _) = Foundation.HomeR
-- OK button pressed;  work out which one and score this item
enaction (Nothing, Just _, _) = Foundation.ScoreR
-- "logout" button pressed;  so do it.
enaction (Nothing, Nothing, Just _) = Foundation.AuthR YA.LogoutR
-- "this should never happen";  not sure what to do here.
enaction (Nothing, Nothing, Nothing) = Foundation.HomeR
