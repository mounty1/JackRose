{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}

{-|
Description: Item review pages;  show page and take user score
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

module Review (getHomeR, postHomeR) where


import qualified Yesod as Y
import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Control.Applicative as CA ((<$>), (<*>))
import qualified Foundation
import qualified Data.Text as DT (Text)
import qualified Data.Maybe as DM
import qualified ReviewPage (review)


getHomeR, postHomeR :: Foundation.Handler Y.Html

getHomeR = YA.maybeAuthId >>= DM.maybe loginPlease ReviewPage.review
-- ^ verify that a user be logged-in, and if s/he be, present the next item for review.

postHomeR = YA.maybeAuthId >>= DM.maybe loginPlease score
-- ^ user has scored their item so re-schedule it and move to the next.


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
-- | user has pressed a 'score' button; update database with new review and go to next item
score :: DT.Text -> Foundation.Handler YC.Html
score username =
	(Y.runInputPost $ triple CA.<$> Y.iopt Y.textField "load" CA.<*> Y.iopt Y.textField "grade" CA.<*> Y.iopt Y.textField "logout") >>= enaction


type OpText = Maybe DT.Text


triple :: OpText -> OpText -> OpText -> (OpText, OpText, OpText)
triple one two three = (one, two, three)


enaction :: (OpText, OpText, OpText) -> Foundation.Handler YC.Html
-- "load" button pressed;  go to upload/download screen (not yet written)
enaction (Just _, _, _) = goHome
-- one of the grade buttons pressed;  work out which one and score this item
enaction (Nothing, Just grade, _) = goHome
-- "logout" button pressed;  so do it.
enaction (Nothing, Nothing, Just _) = YC.redirect (Foundation.AuthR YA.LogoutR)
-- "this should never happen";  not sure what to do here.
enaction (Nothing, Nothing, Nothing) = goHome


loginPlease, goHome :: Foundation.Handler YC.Html

goHome = YC.redirect Foundation.HomeR

loginPlease = YC.redirect (Foundation.AuthR YA.LoginR)
