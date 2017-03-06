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
import LoginPlease (onlyIfAuthorised)
import GoHome (goHome)
import ScoreGet (goScoreR)


-- | user has scored their item so re-schedule it and move to the next.
postHomeR :: Foundation.Handler YC.Html
postHomeR = onlyIfAuthorised reveal


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
-- | user has pressed a 'score' button; update database with new review and go to next item
reveal :: Text -> Foundation.Handler YC.Html
reveal username =
	(Y.runInputPost $ triple <$> Y.iopt Y.textField "stats" <*> Y.iopt Y.textField "OK" <*> Y.iopt Y.textField "logout") >>= enaction username


type OpText = Maybe Text


triple :: OpText -> OpText -> OpText -> (OpText, OpText, OpText)
triple one two three = (one, two, three)


enaction :: Text -> (OpText, OpText, OpText) -> Foundation.Handler YC.Html
-- "stats" button pressed;  go to upload/download screen (not yet written)
enaction _ (Just _, _, _) = goHome
-- OK button pressed;  work out which one and score this item
enaction username (Nothing, Just grade, _) = goScoreR username
-- "logout" button pressed;  so do it.
enaction _ (Nothing, Nothing, Just _) = YC.redirect (Foundation.AuthR YA.LogoutR)
-- "this should never happen";  not sure what to do here.
enaction _ (Nothing, Nothing, Nothing) = goHome
