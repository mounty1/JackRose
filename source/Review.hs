{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Review(review, score) where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Control.Applicative as CA ((<$>), (<*>))
import qualified Foundation (JRState(..))
import qualified Pervasive (TextItem)
import qualified Routing


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
score :: Pervasive.TextItem -> YC.HandlerT Foundation.JRState IO YC.Html
score username =
	(Y.runInputPost $ triple CA.<$> Y.ireq Y.textField "login" CA.<*> Y.ireq Y.textField "username" CA.<*> Y.iopt Y.textField "password") >>= enaction


triple :: Pervasive.TextItem -> Pervasive.TextItem -> Maybe Pervasive.TextItem -> (Pervasive.TextItem, Pervasive.TextItem, Maybe Pervasive.TextItem)
triple one two three = (one, two, three)


enaction :: (Pervasive.TextItem, Pervasive.TextItem, Maybe Pervasive.TextItem) -> YC.HandlerT Foundation.JRState IO YC.Html
enaction ("new", username, _) = do
	YC.redirect Routing.HomeR
enaction ("login", username, Just password) = do
	YC.redirect Routing.HomeR
enaction (_, username, _) = do
	YC.redirect Routing.HomeR


review :: Pervasive.TextItem -> YC.HandlerT Foundation.JRState IO YC.Html
review username = YC.defaultLayout $(YC.whamletFile "loggedin.hamlet")