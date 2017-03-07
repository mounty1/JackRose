{-|
Description: Return content of next review item
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes, KindSignatures #-}


module ReviewGet (getHomeR, getReviewR) where


import qualified Yesod.Core as YC
import qualified Foundation (Handler)
import qualified FailureMessage (page)
import qualified Data.Text as DT (Text, split, concat, null, pack)
import qualified Text.XML as XML (Document)
import qualified Data.Map as DM
import qualified Data.List as DL (filter)
import LoginPlease (onlyIfAuthorised)
import qualified JRState (runFilteredLoggingT, userConfig, getUserConfig, getDataSchemes, JRState, tablesFile)
import qualified UserDeck (UserDeckCpt(..), NewThrottle)
import LearningData (ViewId, LearnDatum(..), DataRow(..), newItem, dueItem)
import qualified LearningData (get, View(..))
import Data.Time (getCurrentTime)
import Authorisation (UserId)
import Database.Persist.Sqlite (runSqlPool)
import TextShow (showt)
import Database.Persist.Sql (Entity(Entity), fromSqlKey, Key, SqlPersistT, SqlBackend)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import ConnectionSpec (DataDescriptor(..))
import Database.Persist (ToBackendKey)
import CardExpand (expand)
import qualified PresentHTML as PH
import qualified DeckSpec (content)
import UserToId (userToId)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import qualified SessionItemKey (set)
import qualified ExternalData (get)


type PresentationParams = Either DT.Text (Entity LearnDatum, XML.Document)


type LearnItemParameters = forall m. (YC.MonadIO m, YC.MonadBaseControl IO m) => ReaderT SqlBackend m (Maybe PresentationParams)


-- | try to extract the login user name from the session; if present, verify that it's logged-in
getHomeR :: Foundation.Handler YC.Html
getHomeR = onlyIfAuthorised getLoginR


-- | TODO merge into above.
-- TODO sort out all these entry points ... what a mess.  Cf. Foundation and RouteData
getLoginR :: DT.Text -> Foundation.Handler YC.Html
getLoginR acctName = YC.getYesod >>= checkAlreadyLoggedIn acctName


checkAlreadyLoggedIn :: DT.Text  -> JRState.JRState -> Foundation.Handler YC.Html
checkAlreadyLoggedIn acctName site = YC.liftIO (JRState.getUserConfig site) >>= maybe (verifyUser acctName site) (descendToDeckRoot site Nothing []) . DM.lookup acctName


verifyUser :: DT.Text -> JRState.JRState -> Foundation.Handler YC.Html
verifyUser acctName site =YC.liftIO (userToId site acctName) >>= maybe
		(FailureMessage.page $ DT.concat [DT.pack "no user record: ", acctName])
		(\(Entity uid _) -> YC.liftIO (DeckSpec.content uid site) >>= either FailureMessage.page (digest acctName uid site))


digest :: DT.Text -> Authorisation.UserId -> JRState.JRState -> [UserDeck.UserDeckCpt] -> Foundation.Handler YC.Html
digest acctName uid site userSchema =
		(YC.liftIO $ atomically $ modifyTVar' (JRState.userConfig site) (DM.insert acctName (uid, userSchema)))
		>> onlyIfAuthorised (review [])


-- | show next item for review, for the logged-in user
getReviewR :: DT.Text -> Foundation.Handler YC.Html
getReviewR = onlyIfAuthorised . review . DL.filter (not . DT.null) . DT.split (== '/')


review :: [DT.Text] -> DT.Text -> Foundation.Handler YC.Html
review deckPath {- e.g., ["Language", "Alphabets", "Arabic"] -} username = YC.getYesod >>= zappo where
		zappo site = (YC.liftIO $ JRState.getUserConfig site) >>=
			maybe (FailureMessage.page $ DT.concat ["user \"", username, "\" dropped from state"]) (descendToDeckRoot site Nothing deckPath) . DM.lookup username


descendToDeckRoot :: JRState.JRState -> UserDeck.NewThrottle -> [DT.Text] -> (UserId, [UserDeck.UserDeckCpt]) -> Foundation.Handler YC.Html

-- Still nodes to descend in requested sub-tree, but nowhere to go
descendToDeckRoot _ _ _ (_, []) = FailureMessage.page "nowhere to go"

-- If XPath-like spec. is empty then 'stuff' is the requested sub-tree so pick an item to display.
-- This is significant inasmuch as if it be deterministic, there will be no randomness if the user goes away then tries again later.
-- The algorithm must present an item if any be due;  otherwise a 'new' item, constrained by the cascaded throttle,
-- which is the daily (well, sliding 24 hour window) limit on new cards.
descendToDeckRoot site throttle [] (userId, stuff : _) = searchForExistingByTable site userId flattenedDeck >>= maybe fallToNew rememberItem where
		fallToNew = searchForNew site userId throttle flattenedDeck >>= maybe (FailureMessage.page "no more items") rememberItem
		flattenedDeck = tableList stuff

-- both XPath-like and tree;  find a matching node (if it exist) and descend
descendToDeckRoot site throttle deckPath@(d1 : dn) (userId, UserDeck.SubDeck maybeThrottle shuffle label item : rest) =
	if d1 == label then descendToDeckRoot site newThrottle dn (userId, item) else descendToDeckRoot site throttle deckPath (userId, rest) where
		newThrottle = mergeThrottle throttle maybeThrottle

-- no match so try next peer
descendToDeckRoot site throttle deckPath (userId, _ : rest) = descendToDeckRoot site throttle deckPath (userId, rest)


-- TODO document
searchForExistingByTable :: JRState.JRState -> UserId -> [ViewId] -> Foundation.Handler (Maybe PresentationParams)

-- descended to a terminal node (a TableView) so get the next card from it
searchForExistingByTable site userId views =YC.liftIO $ getCurrentTime >>= showCardItem where
	showCardItem now = runItemQuery site (dueItem userId now views)


tableList :: UserDeck.UserDeckCpt -> [ViewId]
tableList (UserDeck.TableView _ _ vid) = [vid]
tableList (UserDeck.SubDeck _ _ _ dex) = concatMap tableList dex


searchForNew ::JRState.JRState -> UserId -> UserDeck.NewThrottle -> [ViewId] -> Foundation.Handler (Maybe PresentationParams)
searchForNew site userId throttle views = runItemQuery site (newItem userId views)


runItemQuery :: (YC.MonadIO m, YC.MonadBaseControl IO m) => JRState.JRState -> SqlPersistT (LoggingT m) (Maybe (Entity LearnDatum)) -> m (Maybe PresentationParams)
runItemQuery site fn = JRState.runFilteredLoggingT site (runSqlPool fn2 (JRState.tablesFile site)) where

	fn2 = fn >>= maybe (return Nothing) getItemAndViewIds

	getItemAndViewIds :: Entity LearnDatum -> LearnItemParameters
	getItemAndViewIds item@(Entity _ (LearnDatum _ itemId _ _ _)) = LearningData.get itemId
			>>= maybe (noSomething "data row" itemId) (formatItem item)

	formatItem :: Entity LearnDatum -> DataRow -> LearnItemParameters
	formatItem item (DataRow key source _) = YC.liftIO (JRState.getDataSchemes site)
			>>= maybe (noSomething "live data source" source) (readFromView item key) . DM.lookup source

	readFromView :: Entity LearnDatum -> DT.Text -> DataDescriptor -> LearnItemParameters
	readFromView item@(Entity _ (LearnDatum viewId _ _ _ _)) key descriptor = LearningData.get viewId
			>>= maybe (noSomething "view" viewId) (readFromSource item key descriptor)


noSomething :: (ToBackendKey SqlBackend record) => DT.Text -> Key record -> LearnItemParameters
noSomething label item = YC.liftIO $ return $ Just $ Left $ DT.concat [label, " lost: ", showt $ fromSqlKey item]


readFromSource :: Entity LearnDatum -> DT.Text -> DataDescriptor ->  LearningData.View -> LearnItemParameters
readFromSource item key (DataDescriptor cols keys1y handle) (LearningData.View _ _ obverse _) =
	YC.liftIO $
		ExternalData.get key keys1y handle
			-- if we get a [XML.Node] back, pack it up;  if a Left error, pass it unchanged.
			>>= return . Just . fmap (((,) item) . PH.documentXHTML PH.okButton) . CardExpand.expand cols Nothing obverse


-- put the necessary data into the session so that the POST knows what to add or update.
-- TODO a Left indicates inconsistency in the data, so really we should log every one out and fix it.
rememberItem :: PresentationParams -> Foundation.Handler YC.Html
rememberItem (Left errCode) = FailureMessage.page errCode
rememberItem (Right (Entity itemId _, document)) = SessionItemKey.set itemId >> PH.toHTMLdoc document


mergeThrottle :: UserDeck.NewThrottle -> UserDeck.NewThrottle -> UserDeck.NewThrottle
mergeThrottle Nothing newThrottle = newThrottle
mergeThrottle already Nothing = already
mergeThrottle a@(Just already) n@(Just newThrottle) = if already < newThrottle then a else n
