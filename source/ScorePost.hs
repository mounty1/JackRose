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
import qualified Data.Text as DT (Text, head)
import qualified JRState (tablesFile)
import GoHome (goHome)
import LearningData (get, insert, History(History), LearnDatum(LearnDatum), updateTimeStamp, lastHistory)
import SessionItemKey (get)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (addUTCTime, diffUTCTime, NominalDiffTime)
import Data.Int (Int8)
import FailureMessage (page)
import Database.Persist.Sqlite (runSqlPool)
import Database.Persist.Sql (SqlBackend)
import Control.Monad.Trans.Reader (ReaderT)
import DespatchButtons (despatch)


-- | user has pressed a 'score' button; update database with new review and go to next item
postScoreR :: Foundation.Handler YC.Html
postScoreR = YA.requireAuthId >> despatch (const $ YC.redirect $ Foundation.AuthR YA.LogoutR) routeTable >>= \action -> SessionItemKey.get >>= action


type ParameteredRoute = DT.Text ->  Maybe (Y.Key LearnDatum) -> Foundation.Handler YC.Html


routeTable :: [( DT.Text, ParameteredRoute)]
routeTable = [
		( "stats", justGoHome ),
		( "grade", writeGrade ),
		( "logout", justLogout )
	]


justGoHome, justLogout, writeGrade :: ParameteredRoute


justGoHome _ _ = goHome


justLogout _ _ = YC.redirect (Foundation.AuthR YA.LogoutR)


writeGrade "" _ = FailureMessage.page "?? null grade ??"
writeGrade _ Nothing = FailureMessage.page "?? item ??"
writeGrade grade (Just itemId) = pearl (scaledGrade $ DT.head grade) itemId


pearl :: Int8 -> Y.Key LearnDatum -> Foundation.Handler YC.Html
-- In the extremely unlikely event of the learning datum being unfound, fail.
-- This would imply a race-condition with it being deleted between review and scoring.
pearl numGrade datumId = YC.getYesod >>= \site -> YC.liftIO (getCurrentTime >>= \time -> runSqlPool (newHistory time) (JRState.tablesFile site)) >>= maybe goHome FailureMessage.page where
	newHistory time = LearningData.get datumId >>= maybe
			(return $ Just "?? learn ??")
			(\learnD -> LearningData.insert (History datumId time numGrade) >> nextReviewDate datumId numGrade time learnD >>= LearningData.updateTimeStamp datumId >> return Nothing)


baseGrade :: Int
baseGrade = fromEnum '0'


-- scales '0' .. '9' from 0 to 126
scaledGrade :: Char -> Int8
scaledGrade grade = fromIntegral $ 14 * (fromEnum grade - baseGrade)


nominalDay :: NominalDiffTime
nominalDay = fromInteger $ 24 * 60 * 60


-- | implement SM2+ algorithm at <http://www.blueraja.com/blog/477/a-better-spaced-repetition-learning-algorithm-sm2>
nextReviewDate :: Y.Key LearnDatum -> Int8 -> UTCTime -> LearnDatum -> ReaderT SqlBackend IO UTCTime
-- This is the point at which to branch on spaceAlgorithm
nextReviewDate datumId numGrade timeNow (LearnDatum _ _ _ _ _ difficulty secsBetweenReviews nextReview) = LearningData.lastHistory 1 datumId >>= return . flip addUTCTime timeNow . reviewIncrement where
	reviewIncrement :: [History] -> NominalDiffTime
	reviewIncrement [] = calculation nominalDay
	reviewIncrement (History _ stamp1 _ : _) = calculation (diffUTCTime timeNow stamp1)
	calculation :: NominalDiffTime -> NominalDiffTime
	calculation deltaTime = realToFrac $ if incorrect then (secsBetweenReviews / difficultyWeight * difficultyWeight) `min` 1.0 else secsBetweenReviews * (1.0 + (difficultyWeight - 1.0)*percentOverdue) where
		percentOverdue :: Double
		percentOverdue = if incorrect then 1.0 else 2.0 `min` realToFrac (deltaTime / diffUTCTime timeNow nextReview)
		difficultyNext = (difficulty * percentOverdue * (8.0 - 9.0 * performanceRating) / 17.0) `max` 1.0 `min` 0.0
		difficultyWeight = 3.0 - 1.7 * difficultyNext
	incorrect = performanceRating < 0.6
	performanceRating :: Double
	performanceRating = fromIntegral numGrade / fromIntegral (scaledGrade '9')
