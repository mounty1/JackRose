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
import Control.Applicative ((<$>), (<*>))
import qualified Foundation
import Data.Text (Text)
import LoginPlease (onlyIfAuthorised)
import GoHome (goHome)


-- | user has scored their item so re-schedule it and move to the next.
postHomeR :: Foundation.Handler YC.Html
postHomeR = onlyIfAuthorised score


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
-- | user has pressed a 'score' button; update database with new review and go to next item
score :: Text -> Foundation.Handler YC.Html
score username =
	(Y.runInputPost $ triple <$> Y.iopt Y.textField "stats" <*> Y.iopt Y.textField "grade" <*> Y.iopt Y.textField "logout") >>= enaction


type OpText = Maybe Text


triple :: OpText -> OpText -> OpText -> (OpText, OpText, OpText)
triple one two three = (one, two, three)


enaction :: (OpText, OpText, OpText) -> Foundation.Handler YC.Html
-- "stats" button pressed;  go to upload/download screen (not yet written)
enaction (Just _, _, _) = goHome
-- one of the grade buttons pressed;  work out which one and score this item
enaction (Nothing, Just grade, _) = goHome
-- "logout" button pressed;  so do it.
enaction (Nothing, Nothing, Just _) = YC.redirect (Foundation.AuthR YA.LogoutR)
-- "this should never happen";  not sure what to do here.
enaction (Nothing, Nothing, Nothing) = goHome
