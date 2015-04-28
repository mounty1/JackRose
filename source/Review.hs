{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}

module Review(review, score) where


import qualified Yesod as Y
import qualified Yesod.Auth as YA  -- for whamletfile
import qualified Yesod.Core as YC
import qualified Control.Applicative as CA ((<$>), (<*>))
import qualified Foundation
import qualified Pervasive (TextItem)


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
score :: Pervasive.TextItem -> YC.HandlerT Foundation.JRState IO YC.Html
score username =
	(Y.runInputPost $ triple CA.<$> Y.ireq Y.textField "login" CA.<*> Y.ireq Y.textField "username" CA.<*> Y.iopt Y.textField "password") >>= enaction


triple :: Pervasive.TextItem -> Pervasive.TextItem -> Maybe Pervasive.TextItem -> (Pervasive.TextItem, Pervasive.TextItem, Maybe Pervasive.TextItem)
triple one two three = (one, two, three)


enaction :: (Pervasive.TextItem, Pervasive.TextItem, Maybe Pervasive.TextItem) -> YC.HandlerT Foundation.JRState IO YC.Html
enaction ("new", username, _) = goHome
enaction ("login", username, Just password) = goHome
enaction (_, username, _) = goHome


goHome :: Foundation.JRHandlerT Y.Html
goHome = YC.redirect Foundation.HomeR


review :: Pervasive.TextItem -> Foundation.JRHandlerT YC.Html
review username = YC.defaultLayout $(YC.whamletFile "loggedin.hamlet")