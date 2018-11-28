{-|
Description: Take post of user score and update history.
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}


module ScorePost (postScoreR) where


import qualified Yesod as Y
import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation
import qualified Data.Text as DT (Text, uncons)
import qualified JRState (tablesFile)
import GoHome (goHome)
import LearningData (get, insert, History(History), LearnDatum(LearnDatum), updateLearnDatum, lastHistory)
import SessionItemKey (get)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (addUTCTime, diffUTCTime, NominalDiffTime)
import Data.Int (Int8)
import FailureMessage (page)
import Database.Persist.Sql (runSqlPool, SqlBackend)
import Control.Monad.Trans.Reader (ReaderT)
import DespatchButtons (despatch)


-- | User has pressed a /score/ button; update database with new review and go to next item.
postScoreR :: Foundation.Handler YC.Html
postScoreR = YA.requireAuthId >> despatch (const $ YC.redirect $ Foundation.AuthR YA.LogoutR) routeTable >>= \action -> SessionItemKey.get >>= action


type ParameteredRoute = DT.Text -> Maybe (Y.Key LearnDatum) -> Foundation.Handler YC.Html


routeTable :: [( DT.Text, ParameteredRoute)]
routeTable = [
		( "stats", justGoHome ),
		( "grade", writeGrade ),
		( "logout", justLogout )
	]


justGoHome, justLogout, writeGrade :: ParameteredRoute


justGoHome _ _ = goHome


justLogout _ _ = YC.redirect (Foundation.AuthR YA.LogoutR)


-- Fail on empty grade text or no learn datum.
-- Neither of thse 'should happen';  their doing so implies something wrong with
-- the browser's session management or POST implementation.
writeGrade grade = maybe
		(FailureMessage.page "?? item ??")
		(maybe (const $ FailureMessage.page "?? null grade ??") (pearl . scaledGrade . fst) (DT.uncons grade))


pearl :: Int8 -> Y.Key LearnDatum -> Foundation.Handler YC.Html
-- In the extremely unlikely event of the learning datum being unfound, fail.
-- This would imply a race-condition with it being deleted between review and scoring.
pearl numGrade datumId = YC.getYesod >>= \site -> YC.liftIO (getCurrentTime >>= \time -> runSqlPool (newHistory time) (JRState.tablesFile site)) >>= maybe goHome FailureMessage.page where
	newHistory time = LearningData.get datumId >>= maybe
			(return $ Just "?? learn ??")
			(\learnD -> nextReviewDate datumId numGrade time learnD >>= uncurry (LearningData.updateLearnDatum datumId)
				>> LearningData.insert (History datumId time numGrade)
				>> return Nothing)


baseGrade :: Int
baseGrade = fromEnum '0'


-- scales '0' .. '9' from 0 to 126
scaledGrade :: Char -> Int8
scaledGrade grade = fromIntegral $ 14 * (fromEnum grade - baseGrade)


nominalDay :: NominalDiffTime
nominalDay = fromInteger $ 24 * 60 * 60


-- | implement SM2+ algorithm at <http://www.blueraja.com/blog/477/a-better-spaced-repetition-learning-algorithm-sm2>
nextReviewDate :: Y.Key LearnDatum -> Int8 -> UTCTime -> LearnDatum -> ReaderT SqlBackend IO (UTCTime, Double)
-- This is the point at which to branch on spaceAlgorithm
nextReviewDate datumId numGrade timeNow (LearnDatum _ _ _ _ _ difficulty nextReview) = fmap reviewIncrement (LearningData.lastHistory 1 datumId) where
	reviewIncrement :: [History] -> (UTCTime, Double)
	-- If there be no learning history for this item, pretend with one day ago
	reviewIncrement [] = calculation (addUTCTime (- nominalDay) timeNow)
	reviewIncrement [History _ stamp1 _] = calculation stamp1
	reviewIncrement _ = error $ "lastHistory 1 returned n"

	calculation :: UTCTime -> (UTCTime, Double)
	calculation dateLastReviewed = (addUTCTime secsBetweenReviewsNext timeNow, difficultyNext) where

		percentOverdue :: Double
		percentOverdue =  if incorrect then 1.0 else 2.0 `min` realToFrac (diffUTCTime timeNow dateLastReviewed / secsBetweenReviews)

		difficultyNext :: Double
		difficultyNext = (difficulty + percentOverdue * (8.0 - 9.0 * performanceRating) / 17.0) `max` 0.0 `min` 1.0

		secsBetweenReviewsNext :: NominalDiffTime
		secsBetweenReviewsNext = (secsBetweenReviews * spacingFactor) `max` nominalDay

		spacingFactor = if incorrect then
				recip $ realToFrac $ difficultyWeight * difficultyWeight
			else
				realToFrac $ 1.0 + (difficultyWeight - 1.0) * percentOverdue

		secsBetweenReviews :: NominalDiffTime
		-- floor 1.0 here in case dateLastReviewed is after nextReview
		secsBetweenReviews = diffUTCTime nextReview dateLastReviewed `max` 1.0

		difficultyWeight :: Double
		difficultyWeight = 3.0 - 1.7 * difficultyNext

	incorrect = performanceRating < 0.6

	performanceRating :: Double
	performanceRating = fromIntegral numGrade / fromIntegral (scaledGrade '9')
