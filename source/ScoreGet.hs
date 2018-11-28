{-|
Description: Return content of next review item
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes #-}


module ScoreGet (getScoreR) where


import qualified Yesod.Core as YC
import qualified Yesod.Auth as YA
import qualified Foundation (Handler)
import qualified Data.Text as DT (Text, concat)
import qualified Data.Map as DM
import qualified JRState (runFilteredLoggingT, getDataSchemes, JRState, tablesFile)
import LearningData (LearnDatum(..), DataRow(..))
import qualified LearningData (get, View(..))
import TextShow (showt)
import Database.Persist.Sql (runSqlPool, fromSqlKey, Key, ToBackendKey, SqlBackend)
import Control.Monad.Trans.Reader (ReaderT)
import GoHome (goHome)
import qualified PresentHTML as PH
import qualified SessionItemKey (get)
import ConnectionSpec (DataDescriptor(..))
import CardExpand (expand)
import ExternalSQL (get)
import qualified CardItemGet (PresentationParams, rememberItem)


type LearnItemParameters = forall m. YC.MonadIO m => ReaderT SqlBackend m CardItemGet.PresentationParams


-- | Process /OK/ button;  first extract item id. from session data
getScoreR :: Foundation.Handler YC.Html
getScoreR = YA.requireAuthId >> SessionItemKey.get >>= maybe goHome showAnswer


showAnswer :: Key LearnDatum -> Foundation.Handler YC.Html
showAnswer itemId = YC.getYesod >>= showAnswer' itemId


showAnswer' :: Key LearnDatum -> JRState.JRState -> Foundation.Handler YC.Html
showAnswer' itemId site = JRState.runFilteredLoggingT site (runSqlPool fn2 (JRState.tablesFile site)) >>= CardItemGet.rememberItem where

	fn2 = LearningData.get itemId >>= maybe (noSomething "item" itemId) getItemAndViewIds

	getItemAndViewIds :: LearnDatum -> LearnItemParameters
	getItemAndViewIds item@(LearnDatum _ rowItemId _ _ _ _ _) = LearningData.get rowItemId
			>>= maybe (noSomething "data row" rowItemId) (formatItem item)

	formatItem :: LearnDatum -> DataRow -> LearnItemParameters
	formatItem item (DataRow key source _) = YC.liftIO (JRState.getDataSchemes site)
			>>= maybe (noSomething "live data source" source) (readFromView item key) . DM.lookup source

	readFromView :: LearnDatum -> DT.Text -> DataDescriptor -> LearnItemParameters
	readFromView (LearnDatum viewId _ _ _ _ _ _) key descriptor = LearningData.get viewId
			>>= maybe (noSomething "view" viewId) (readFromSource itemId key descriptor)


noSomething :: ToBackendKey SqlBackend record => DT.Text -> Key record -> LearnItemParameters
noSomething label item = YC.liftIO $ return $ Left $ DT.concat [label, " lost: ", showt $ fromSqlKey item]


-- TODO should obtain the view deck hierarchy name as well
readFromSource :: Key LearnDatum -> DT.Text -> DataDescriptor -> LearningData.View -> LearnItemParameters
readFromSource itemId key (DataDescriptor cols keys1y handle) (LearningData.View viewName _ obverse reverze style) =
	YC.liftIO $ ExternalSQL.get mash key keys1y handle where
	-- if we get a [XML.Node] back, pack it up;  if a Left error, pass it unchanged.
	mash = fmap ((,) itemId . PH.documentXHTML style viewName PH.gradeButtons) . CardExpand.expand cols (Just obverse) reverze
