{-|
Description: Return content of next review item
Copyright: (c) Michael Mounteney, 2021
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Authorisation checks before going on to present the next review item.
-}


{-# LANGUAGE OverloadedStrings #-}


module ReviewGet (getHomeR, getReviewR) where


import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Foundation
import qualified FailureMessage (page)
import qualified Data.Text as DT (Text, split, concat, null)
import qualified Data.Map as DM
import qualified Data.List as DL (filter)
import qualified JRState (userConfig, getUserConfig, JRState)
import qualified UserDeck (UserDeckCpt(..))
import Database.Persist.Sql (Entity(Entity))
import Authorisation (UserId)
import qualified DeckSpec (content)
import UserToId (userToId)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import qualified ReviewShow (showAtRoot)


-- | Try to extract the login user name from the session; if present, verify that it's logged-in.
getHomeR :: Foundation.Handler YC.Html
getHomeR = YA.requireAuthId >>= (YC.getYesod >>=) . checkAlreadyLoggedIn


checkAlreadyLoggedIn :: DT.Text -> JRState.JRState -> Foundation.Handler YC.Html
checkAlreadyLoggedIn acctName site = YC.liftIO (JRState.getUserConfig site) >>= maybe (verifyUser acctName site) (ReviewShow.showAtRoot site []) . DM.lookup acctName


verifyUser :: DT.Text -> JRState.JRState -> Foundation.Handler YC.Html
verifyUser acctName site =YC.liftIO (userToId site acctName) >>= maybe
		(FailureMessage.page $ DT.concat ["no user record: ", acctName])
		(\(Entity uid _) -> YC.liftIO (DeckSpec.content uid site) >>= either FailureMessage.page (digest acctName uid site))


digest :: DT.Text -> Authorisation.UserId -> JRState.JRState -> [UserDeck.UserDeckCpt] -> Foundation.Handler YC.Html
digest acctName uid site userSchema =
		YC.liftIO (atomically $ modifyTVar' (JRState.userConfig site) (DM.insert acctName (uid, userSchema)))
		>> present [] acctName site


-- | Show next item for review, for the logged-in user.
getReviewR :: DT.Text -> Foundation.Handler YC.Html
-- | break up deck string into components;  add state singleton;  add logged-in user;  review
getReviewR = (YA.requireAuthId >>=) . ((YC.getYesod >>=) .) . present . DL.filter (not . DT.null) . DT.split ('/' ==)


present :: [DT.Text] -> DT.Text -> JRState.JRState -> Foundation.Handler YC.Html
present deckPath {- e.g., ["Language", "Alphabets", "Arabic"] -} username site = YC.liftIO (JRState.getUserConfig site) >>=
	maybe (FailureMessage.page $ DT.concat ["user \"", username, "\" dropped from state"]) (ReviewShow.showAtRoot site deckPath) . DM.lookup username
