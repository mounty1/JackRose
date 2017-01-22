{-|
Description: Post-login processing:  load account name and credentials into session
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module LoginPost (getLoginR, getLoginPostR) where


import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation (Handler)
import qualified Data.Text as DT (Text, pack, concat)
import qualified Data.Map as DM (insert)
import qualified DeckSpec (content)
import UserDeck (UserDeckCpt)
import qualified FailureMessage (page)
import qualified JRState (JRState(..))
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import ReviewGet (getHomeR)
import Database.Persist.Sql as DPQ (Entity(Entity))
import Authorisation (UserId)
import UserToId (userToId)


getLoginPostR :: Foundation.Handler YC.Html
getLoginPostR = YC.lookupSession YA.credsKey >>= maybe getHomeR getLoginR


getLoginR :: DT.Text -> Foundation.Handler YC.Html
getLoginR acctName = YC.getYesod >>= \site -> YC.liftIO (userToId site acctName) >>= maybe
		(FailureMessage.page $ DT.concat [DT.pack "no user record: ", acctName])
		(\(DPQ.Entity uid _) -> YC.liftIO (DeckSpec.content uid site) >>= either FailureMessage.page (digest acctName uid site))


digest :: DT.Text -> Authorisation.UserId -> JRState.JRState -> [UserDeckCpt] -> Foundation.Handler YC.Html

digest acctName uid site userSchema =
		(YC.liftIO $ atomically $ modifyTVar' (JRState.userConfig site) (DM.insert acctName (uid, userSchema)))
		>> getHomeR
