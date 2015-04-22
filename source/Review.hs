{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses #-}

module Review() where


import qualified Yesod as Y
import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Yesod.Auth.Account as YAA
import qualified Control.Applicative as CA ((<$>), (<*>))
import qualified Foundation (JRState(..))
import qualified Pervasive (TextItem, fromByteS, length)



-- http://lusku.de/blog/entry/1 for how to handle grade buttons
postHomeR :: JRHandlerT ()
postHomeR =
	(Y.runInputPost $ triple CA.<$> Y.ireq Y.textField "login" CA.<*> Y.ireq Y.textField "username" CA.<*> Y.iopt Y.textField "password") >>= enaction


triple :: Pervasive.TextItem -> Pervasive.TextItem -> Maybe Pervasive.TextItem -> (Pervasive.TextItem, Pervasive.TextItem, Maybe Pervasive.TextItem)
triple one two three = (one, two, three)


enaction :: (Pervasive.TextItem, Pervasive.TextItem, Maybe Pervasive.TextItem) -> JRHandlerT ()
enaction ("new", username, _) = do
	YC.redirect (AuthR YAA.resetPasswordR)
enaction ("login", username, Just password) = do
	YC.redirect HomeR
enaction (_, username, _) = do
	YC.redirect HomeR
