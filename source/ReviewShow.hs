{-|
Description: Return content of next review item
Copyright: (c) Michael Mounteney, 2021
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Present the next review item;  to be called only after authorisation is obtained.
-}


{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes #-}


module ReviewShow (showAtRoot) where


import qualified Yesod.Core as YC
import qualified Foundation
import qualified FailureMessage (page)
import qualified Data.Text as DT (Text, concat)
import qualified Data.Map as DM
import qualified Data.List as DL (intersperse)
import qualified JRState (runFilteredLoggingT, getDataSchemes, JRState, tablesFile)
import qualified UserDeck (UserDeckCpt(..), NewThrottle)
import LearningData (ViewId, LearnDatum(..), DataRow(..), newItem, dueItem)
import qualified LearningData (get, View(..))
import Data.Time (getCurrentTime)
import Authorisation (UserId)
import TextShow (showt)
import Database.Persist.Sql (runSqlPool, Entity(Entity), fromSqlKey, Key, SqlPersistT, SqlBackend)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import ConnectionSpec (DataDescriptor(..))
import Database.Persist (ToBackendKey)
import CardExpand (expand)
import qualified PresentHTML as PH
import qualified ExternalSQL (get)
import qualified CardItemGet (PresentationParams, rememberItem)


showAtRoot :: JRState.JRState -> [DT.Text] -> (UserId, [UserDeck.UserDeckCpt]) -> Foundation.Handler YC.Html
showAtRoot site deckPath userNodes = descendToDeckRoot site Nothing deckPath [] userNodes


descendToDeckRoot :: JRState.JRState -> UserDeck.NewThrottle -> [DT.Text] -> [DT.Text] -> (UserId, [UserDeck.UserDeckCpt]) -> Foundation.Handler YC.Html

-- Still nodes to descend in requested sub-tree, but nowhere to go
descendToDeckRoot _ _ _ _ (_, []) = FailureMessage.page "nowhere to go"

-- If XPath-like spec. is empty then 'stuff' is the requested sub-tree so pick an item to display.
-- This is significant inasmuch as if it be deterministic, there will be no randomness if the user goes away then tries again later.
-- The algorithm must present an item if any be due;  otherwise a 'new' item, constrained by the cascaded throttle,
-- which is the daily (well, sliding 24 hour window) limit on new cards.
descendToDeckRoot site throttle [] deckDrilled (userId, stuff) = searchForExistingByTable site userId flattenedDeck deckDrilled >>= maybe fallToNew CardItemGet.rememberItem where
		fallToNew = searchForNew site userId throttle flattenedDeck deckDrilled >>= maybe (YC.redirect $ Foundation.NoticeR "No more reviews due") CardItemGet.rememberItem
		flattenedDeck = concatMap tableList stuff

-- both XPath-like and tree;  find a matching node (if it exist) and descend
descendToDeckRoot site throttle deckPath@(d1 : dn) deckDrilled (userId, UserDeck.SubDeck maybeThrottle shuffle label item : rest) =
	if d1 == label then descendToDeckRoot site newThrottle dn (d1 : deckDrilled) (userId, item) else descendToDeckRoot site throttle deckPath deckDrilled (userId, rest) where
		newThrottle = mergeThrottle throttle maybeThrottle

-- no match so try next peer
descendToDeckRoot site throttle deckPath deckDrilled (userId, _ : rest) = descendToDeckRoot site throttle deckPath deckDrilled (userId, rest)


-- TODO document
searchForExistingByTable :: JRState.JRState -> UserId -> [ViewId] -> [DT.Text] -> Foundation.Handler (Maybe CardItemGet.PresentationParams)
-- | descended to a terminal node (a TableView) so get the next card from it
searchForExistingByTable site userId views deckDrilled =YC.liftIO $ getCurrentTime >>= showCardItem where
	showCardItem now = runItemQuery site (dueItem userId now views) deckDrilled


tableList :: UserDeck.UserDeckCpt -> [ViewId]
tableList (UserDeck.TableView _ _ vid) = [vid]
tableList (UserDeck.SubDeck _ _ _ dex) = concatMap tableList dex


searchForNew ::JRState.JRState -> UserId -> UserDeck.NewThrottle -> [ViewId] -> [DT.Text] -> Foundation.Handler (Maybe CardItemGet.PresentationParams)
searchForNew site userId throttle views = runItemQuery site (newItem userId views)


type LearnItemParameters = forall m. YC.MonadIO m => ReaderT SqlBackend m (Maybe CardItemGet.PresentationParams)


runItemQuery :: (YC.MonadIO m, YC.MonadBaseControl IO m) => JRState.JRState -> SqlPersistT (LoggingT m) (Maybe (Entity LearnDatum)) -> [DT.Text] -> m (Maybe CardItemGet.PresentationParams)
runItemQuery site fn deckDrilled = JRState.runFilteredLoggingT site (runSqlPool fn2 (JRState.tablesFile site)) where

	fn2 = fn >>= maybe (return Nothing) getItemAndViewIds

	getItemAndViewIds :: Entity LearnDatum -> LearnItemParameters
	getItemAndViewIds item@(Entity _ (LearnDatum _ itemId _ _ _ _ _)) = LearningData.get itemId
			>>= maybe (noSomething "data row" itemId) (formatItem item)

	formatItem :: Entity LearnDatum -> DataRow -> LearnItemParameters
	formatItem item (DataRow key source _) = YC.liftIO (JRState.getDataSchemes site)
			>>= maybe (noSomething "live data source" source) (readFromView item key) . DM.lookup source

	readFromView :: Entity LearnDatum -> DT.Text -> DataDescriptor -> LearnItemParameters
	readFromView (Entity ikey (LearnDatum viewId _ _ _ _ _ _)) key descriptor = LearningData.get viewId
			>>= maybe (noSomething "view" viewId) (readFromSource ikey key deckDrilled descriptor)


noSomething :: ToBackendKey SqlBackend record => DT.Text -> Key record -> LearnItemParameters
noSomething label item = YC.liftIO $ return $ Just $ Left $ DT.concat [label, " lost: ", showt $ fromSqlKey item]


readFromSource :: Key LearnDatum -> DT.Text -> [DT.Text] -> DataDescriptor -> LearningData.View -> LearnItemParameters
readFromSource item key deckDrilled (DataDescriptor cols keys1y handle) (LearningData.View viewName _ obverse _ style) =
	YC.liftIO $ ExternalSQL.get mash key keys1y handle where
	-- if we get a [XML.Node] back, pack it up;  if a Left error, pass it unchanged.
	mash = Just . fmap ((,) item . PH.documentXHTML style (DT.concat $ DL.intersperse "/" (if null deckDrilled then [] else reverse deckDrilled ++ [":"]) ++ [viewName]) PH.okButton) . CardExpand.expand cols Nothing obverse


mergeThrottle :: UserDeck.NewThrottle -> UserDeck.NewThrottle -> UserDeck.NewThrottle
mergeThrottle Nothing newThrottle = newThrottle
mergeThrottle already Nothing = already
mergeThrottle a@(Just already) n@(Just newThrottle) = if already < newThrottle then a else n
