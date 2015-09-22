{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Item review pages;  show page and take user score
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

module Review (getHomeR, postHomeR, getLoginR) where


import qualified Yesod as Y
import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import Control.Applicative ((<$>), (<*>))
import qualified Foundation
import Data.Text (Text)
import qualified ReviewPage (review)
import qualified AuthoriStyle (Style(..))
import qualified Login (load)


getHomeR, postHomeR :: Foundation.Handler Y.Html

getHomeR = YA.maybeAuthId >>= maybe loginPlease ReviewPage.review
-- ^ verify that a user be logged-in, and if s/he be, present the next item for review.

postHomeR = YA.maybeAuthId >>= maybe loginPlease score
-- ^ user has scored their item so re-schedule it and move to the next.


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
-- | user has pressed a 'score' button; update database with new review and go to next item
score :: Text -> Foundation.Handler YC.Html
score username =
	(Y.runInputPost $ triple <$> Y.iopt Y.textField "load" <*> Y.iopt Y.textField "grade" <*> Y.iopt Y.textField "logout") >>= enaction


type OpText = Maybe Text


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


getLoginR :: Text -> Foundation.Handler YC.Html
getLoginR acctName = YC.getYesod >>= setAcctIfTrusted acctName


setAcctIfTrusted :: Text -> Foundation.JRState -> Foundation.Handler YC.Html
setAcctIfTrusted acctName site = setAcctIfTrusted' acctName (Foundation.howAuthorised site)


setAcctIfTrusted' :: Text -> AuthoriStyle.Style -> Foundation.Handler YC.Html
setAcctIfTrusted' acctName AuthoriStyle.Email = goHome
setAcctIfTrusted' acctName AuthoriStyle.Trust = Login.load acctName >> goHome


loginPlease, goHome :: Foundation.Handler YC.Html

goHome = YC.redirect Foundation.HomeR

loginPlease = YC.redirect (Foundation.AuthR YA.LoginR)
