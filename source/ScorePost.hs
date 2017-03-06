{-|
Description: Take post of user score and update history.
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module ScorePost (postScoreR) where


import qualified Yesod as Y
import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation
import qualified Data.Text as DT (Text, null, head)
import qualified JRState (tablesFile)
import LoginPlease (onlyIfAuthorised)
import GoHome (goHome)
import LearningData (insert, History(..), LearnDatum, updateTimeStamp)
import SessionItemKey (get)
import Data.Time (getCurrentTime)
import Data.Time.Clock (addUTCTime)
import Data.Int (Int8)
import FailureMessage (page)
import Database.Persist.Sqlite (runSqlPool)


-- | user has scored their item so re-schedule it and move to the next.
postScoreR :: Foundation.Handler YC.Html
postScoreR = onlyIfAuthorised score


-- http://lusku.de/blog/entry/1 for how to handle grade buttons
-- | user has pressed a 'score' button; update database with new review and go to next item
score :: DT.Text -> Foundation.Handler YC.Html
score _ = (Y.runInputPost $ triple <$> Y.iopt Y.textField "stats" <*> Y.iopt Y.textField "grade" <*> Y.iopt Y.textField "logout") >>= \action -> SessionItemKey.get >>= enaction action


type OpText = Maybe DT.Text


type MaybeKey = Maybe (Y.Key LearnDatum)


triple :: OpText -> OpText -> OpText -> (OpText, OpText, OpText)
triple one two three = (one, two, three)


enaction :: (OpText, OpText, OpText) -> MaybeKey -> Foundation.Handler YC.Html
-- "stats" button pressed;  go to upload/download screen (not yet written)
enaction (Just _, _, _) _ = goHome
-- one of the grade buttons pressed;  work out which one and score this item
enaction (Nothing, Just grade, _) itemId = writeGrade grade itemId
-- "logout" button pressed;  so do it.
enaction (Nothing, Nothing, Just _) _ = YC.redirect (Foundation.AuthR YA.LogoutR)
-- "this should never happen";  not sure what to do here.
enaction (Nothing, Nothing, Nothing) _ = goHome


writeGrade :: DT.Text -> MaybeKey -> Foundation.Handler YC.Html
writeGrade grade itemId = if DT.null grade then
		FailureMessage.page "?? null grade ??"
	else
		maybe (FailureMessage.page "?? item ??") (pearl numGrade) itemId where
	numGrade = fromIntegral $ 14 * (fromEnum (DT.head grade) - baseGrade)


pearl :: Int8 -> Y.Key LearnDatum -> Foundation.Handler YC.Html
pearl numGrade datumId = YC.getYesod >>= \site -> YC.liftIO (getCurrentTime >>= \time -> runSqlPool (newHistory time) (JRState.tablesFile site)) >> goHome where
	newHistory time = LearningData.insert (History datumId time numGrade) >> LearningData.updateTimeStamp datumId (addUTCTime (realToFrac numGrade * 11) time)


baseGrade :: Int
baseGrade = fromEnum '0'
