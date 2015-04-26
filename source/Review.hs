{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}

module Review(review, score) where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Control.Applicative as CA ((<$>), (<*>))
import Foundation (JRState(..), goHome)
import qualified Pervasive (TextItem)


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
score :: Pervasive.TextItem -> YC.HandlerT Foundation.JRState IO YC.Html
score username =
	(Y.runInputPost $ triple CA.<$> Y.ireq Y.textField "login" CA.<*> Y.ireq Y.textField "username" CA.<*> Y.iopt Y.textField "password") >>= enaction


triple :: Pervasive.TextItem -> Pervasive.TextItem -> Maybe Pervasive.TextItem -> (Pervasive.TextItem, Pervasive.TextItem, Maybe Pervasive.TextItem)
triple one two three = (one, two, three)


enaction :: (Pervasive.TextItem, Pervasive.TextItem, Maybe Pervasive.TextItem) -> YC.HandlerT Foundation.JRState IO YC.Html
enaction ("new", username, _) = Foundation.goHome
enaction ("login", username, Just password) = Foundation.goHome
enaction (_, username, _) = Foundation.goHome


review :: Pervasive.TextItem -> YC.HandlerT Foundation.JRState IO YC.Html
review username = YC.defaultLayout $(YC.whamletFile "loggedin.hamlet")