{-|
Description: Management of persistent storage;  items learning, review and scoring data.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

DataSource: reference to some database table or flat file.
DataRow:  in Anki parlance, a note;  one table row.
View: 'card':  display information * data row
LearnDatum:  card * datarow * user.
History:  record of item scores.
-}


{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances, DeriveGeneric #-}
{-# OPTIONS_HADDOCK prune #-}


module LearningData (migrateData,
		DataSourceId,
		DataSource(..),
		DataRow(..),
		View(..),
		ViewId,
		DataRowId,
		LearnDatumId,
		LearnDatum(..),
		dueItem,
		newItem,
		History(..),
		Y.get,
		Y.insert,
		updateTimeStamp,
		deleteItems,
		allSourceKeys,
		mkLearnDatum,
		mkLearnDatumKey,
		viewsOnDataSource,
		lastHistory) where


import qualified Yesod as Y
import qualified Data.Text as DT (Text)
import Authorisation (UserId)
import Data.Int (Int8)
import Data.Time (UTCTime)
import Database.Persist (selectList, update, (=.), (<.), (==.), (<-.), SelectOpt(LimitTo, Desc), deleteCascadeWhere)
import Database.Persist.Sql (SqlBackend)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Types (entityKey, entityVal)
import SplitList (split)
import Data.Maybe (listToMaybe)


Y.share [Y.mkPersist Y.sqlSettings, Y.mkDeleteCascade Y.sqlSettings, Y.mkMigrate "migrateData"] [Y.persistLowerCase|
DataSource
	accessorWrite UserId NOT NULL
	accessorRead UserId NOT NULL
	name DT.Text UNIQUE NOT NULL
	sourceSerial DT.Text UNIQUE NOT NULL
	resynced UTCTime NOT NULL
DataRow
	tableKey DT.Text NOT NULL
	dataSourceRowId DataSourceId NOT NULL
	loaded UTCTime NOT NULL
	UniqueRow tableKey dataSourceRowId
View
	name DT.Text NOT NULL
	dataSourceId DataSourceId NOT NULL
	obverse DT.Text NOT NULL
	reverse DT.Text NOT NULL
	styleCSS DT.Text Maybe
LearnDatum
	viewUID ViewId NOT NULL
	itemId DataRowId NOT NULL
	user UserId NOT NULL
	activity Int8 NOT NULL
	spaceAlgorithm Int8 NOT NULL
	factor1 Double NOT NULL
	factor2 Double NOT NULL
	nextReview UTCTime NOT NULL
	LearnItem viewUID itemId user
History
	item LearnDatumId NOT NULL
	stamp UTCTime NOT NULL
	grade Int8 NOT NULL
	Primary item stamp
|]

{- Activity:
	0 new item
	1 suspended by user
	2 active
	3 suppressed by selection criteria
-}

{- For LearnDatum, the 'factor' data have a meaning determined by the spacing algorithm;
    in a PL the algorithm and factors would be a Data type or variant record.
    For the full horror, see ScorePost.hs -}

type PersistResult a = forall (m :: * -> *). Y.MonadIO m => ReaderT SqlBackend m a

type OneLearnPersist = PersistResult (Maybe (Y.Entity LearnDatum))


nextItem :: Int8 -> [Y.Filter LearnDatum] -> UserId -> [Y.Key View] -> OneLearnPersist
-- It's a good idea to keep listToMaybe with LimitTo 1 because they make sense together.
nextItem activityState extras user views = listToMaybe `fmap` selectList ((LearnDatumActivity ==. activityState) : (LearnDatumUser ==. user) : (LearnDatumViewUID <-. views) : extras) [ LimitTo 1 ]


-- | Return the first new item for review, if there be one.
newItem :: UserId -> [Y.Key View] -> OneLearnPersist
newItem = nextItem 0 []


-- | Return the next item for review, if there be one.
dueItem :: UserId -> UTCTime -> [Y.Key View] -> OneLearnPersist
dueItem user stamp = nextItem 2 [ LearnDatumNextReview <. stamp ] user


-- | All data rows referring to the given external data source.
allSourceKeys :: Y.Key DataSource -> PersistResult [DT.Text]
allSourceKeys dsId = map pickKey `fmap` selectList [ DataRowDataSourceRowId ==. dsId ] []


pickKey :: Y.Entity DataRow -> DT.Text
pickKey (Y.Entity _ (DataRow key _ _)) = key


-- | Replace the next-review time-stamp on a learn datum.
updateTimeStamp :: forall (m :: * -> *). Y.MonadIO m => Y.Key LearnDatum -> UTCTime -> ReaderT SqlBackend m ()
updateTimeStamp key time = update key [ LearnDatumNextReview =. time, LearnDatumActivity =. 2 ]


-- | Wrap non-exportable @LearnItem@.
mkLearnDatumKey :: ViewId -> DataRowId -> UserId -> Y.Unique LearnDatum
mkLearnDatumKey = LearnItem


-- | Wrap non-exportable @LearnDatum@.
mkLearnDatum :: ViewId -> Y.Key DataRow -> UserId -> Int8 -> Int8 -> Double -> Double -> UTCTime -> LearnDatum
mkLearnDatum = LearnDatum


-- | Delete items that have been removed from the external data source,
-- and all subordinate data.
deleteItems :: forall (m :: * -> *). Y.MonadIO m => [DT.Text] -> ReaderT (Y.PersistEntityBackend DataRow) m [()]
-- break up the deletion into chunks in order not to have too many variables in the SQL statement
deleteItems deletees = mapM (\portion -> deleteCascadeWhere [DataRowTableKey <-. portion]) $ split 10 deletees


-- | return list of views that refer to the given data source
viewsOnDataSource :: Y.Key DataSource -> PersistResult [Y.Key View]
viewsOnDataSource sourceId = map entityKey `fmap` selectList [ ViewDataSourceId ==. sourceId ] []


-- | Most recent history for the given learn datum
lastHistory :: (Y.PersistQueryRead SqlBackend) => Int -> Y.Key LearnDatum ->PersistResult [History]
lastHistory limit item = map entityVal `fmap` selectList [ HistoryItem ==. item ] [ Desc HistoryStamp, LimitTo limit ]
