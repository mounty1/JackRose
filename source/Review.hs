{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}

{-|
Description: Item review pages;  show page and take user score
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

module Review (review, score) where


import qualified Yesod as Y
import qualified Yesod.Auth as YA  -- for whamletfile
import qualified Yesod.Core as YC
import qualified Control.Applicative as CA ((<$>), (<*>))
import qualified Foundation
import qualified TextItem (TextItem)


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
-- | user has pressed a 'score' button; update database with new review and go to next item
score :: TextItem.TextItem -> Foundation.JRHandlerT YC.Html
score username =
	(Y.runInputPost $ triple CA.<$> Y.ireq Y.textField "login" CA.<*> Y.ireq Y.textField "username" CA.<*> Y.iopt Y.textField "password") >>= enaction


triple :: TextItem.TextItem -> TextItem.TextItem -> Maybe TextItem.TextItem -> (TextItem.TextItem, TextItem.TextItem, Maybe TextItem.TextItem)
triple one two three = (one, two, three)


enaction :: (TextItem.TextItem, TextItem.TextItem, Maybe TextItem.TextItem) -> Foundation.JRHandlerT YC.Html
enaction ("new", username, _) = goHome
enaction ("login", username, Just password) = goHome
enaction (_, username, _) = goHome


goHome :: Foundation.JRHandlerT Y.Html
goHome = YC.redirect Foundation.HomeR


-- | show next item for review, for the logged-in user
review :: TextItem.TextItem -> Foundation.JRHandlerT YC.Html
review username = YC.defaultLayout $(YC.whamletFile "loggedin.hamlet")