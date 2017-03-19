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
import qualified Data.Text as DT (Text, null, head)
import qualified JRState (tablesFile)
import GoHome (goHome)
import LearningData (insert, History(..), LearnDatum, updateTimeStamp, lastHistory)
import SessionItemKey (get)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock (addUTCTime, diffUTCTime, NominalDiffTime)
import Data.Int (Int8)
import FailureMessage (page)
import Database.Persist.Sqlite (runSqlPool)
import Database.Persist.Sql (SqlBackend)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Ratio ((%))
import DespatchButtons (despatch)


-- | user has pressed a 'score' button; update database with new review and go to next item
postScoreR :: Foundation.Handler YC.Html
postScoreR = YA.requireAuthId >> despatch (const $ YC.redirect $ Foundation.AuthR YA.LogoutR) routeTable >>= \action -> SessionItemKey.get >>= action


type ParameteredRoute = MaybeKey -> Foundation.Handler YC.Html


routeTable :: [( DT.Text, DT.Text -> ParameteredRoute)]
routeTable = [
		( "stats", justGoHome ),
		( "grade", writeGrade ),
		( "logout", justLogout )
	]


type MaybeKey = Maybe (Y.Key LearnDatum)


justGoHome, justLogout, writeGrade :: DT.Text -> ParameteredRoute

justGoHome _ _ = goHome

justLogout _ _ = YC.redirect (Foundation.AuthR YA.LogoutR)


writeGrade grade itemId = if DT.null grade then
		FailureMessage.page "?? null grade ??"
	else
		maybe (FailureMessage.page "?? item ??") (pearl $ scaledGrade $ DT.head grade) itemId


pearl :: Int8 -> Y.Key LearnDatum -> Foundation.Handler YC.Html
pearl numGrade datumId = YC.getYesod >>= \site -> YC.liftIO (getCurrentTime >>= \time -> runSqlPool (newHistory time) (JRState.tablesFile site)) >> goHome where
	newHistory time = LearningData.insert (History datumId time numGrade) >> nextReviewDate datumId numGrade time >>= LearningData.updateTimeStamp datumId


baseGrade :: Int
baseGrade = fromEnum '0'


-- scales '0' .. '9' from 0 to 126
scaledGrade :: Char -> Int8
scaledGrade grade = fromIntegral $ 14 * (fromEnum grade - baseGrade)


averageGrade :: Int8
averageGrade = scaledGrade '8'


nominalDay :: NominalDiffTime
nominalDay = fromInteger $ 24 * 60 * 60


nextReviewDate :: Y.Key LearnDatum -> Int8 -> UTCTime -> ReaderT SqlBackend IO UTCTime
nextReviewDate learnId numGrade timeNow = LearningData.lastHistory 1 learnId >>= return . flip addUTCTime timeNow . reviewIncrement where
	reviewIncrement :: [History] -> NominalDiffTime
	reviewIncrement [] = calculation nominalDay numGrade averageGrade
	reviewIncrement (History _ stamp1 grade1 : _) = calculation (diffUTCTime timeNow stamp1) numGrade grade1
	calculation a b c = fromRational $ toRational (a) * (toInteger b % toInteger c)
