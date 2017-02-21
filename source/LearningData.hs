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
		deleteItems,
		ascendingSourceKeys,
		mkLearnDatum,
		mkLearnDatumKey,
		viewsOnDataSource) where


import qualified Yesod as Y
import qualified Data.Text as DT (Text)
import Authorisation (UserId)
import Data.Int (Int8)
import Data.Time (UTCTime)
import Database.Persist (selectList, (<.), (==.), (<-.), SelectOpt(LimitTo, Asc), deleteCascadeWhere)
import Database.Persist.Sql (SqlBackend)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Types (entityKey)
import SplitList (split)


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
LearnDatum
	viewUID ViewId NOT NULL
	itemId DataRowId NOT NULL
	user UserId NOT NULL
	activity Int8 NOT NULL
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

type PersistResult a = forall (m :: * -> *). Y.MonadIO m => ReaderT SqlBackend m a


nextItem :: Int8 -> [Y.Filter LearnDatum] -> UserId -> [Y.Key View] -> PersistResult [Y.Entity LearnDatum]
nextItem activityState extras user views = selectList ((LearnDatumActivity ==. activityState) : (LearnDatumUser ==. user) : (LearnDatumViewUID <-. views) : extras) [ LimitTo 1 ]


newItem :: UserId -> [Y.Key View] -> PersistResult [Y.Entity LearnDatum]
newItem user views = nextItem 0 [] user views


dueItem :: UserId -> UTCTime -> [Y.Key View] -> PersistResult [Y.Entity LearnDatum]
dueItem user stamp views = nextItem 2 [ LearnDatumNextReview <. stamp ] user views


ascendingSourceKeys :: Y.Key DataSource -> PersistResult [DT.Text]
ascendingSourceKeys dsId = map pickKey `fmap` (selectList [ DataRowDataSourceRowId ==. dsId ] [Asc DataRowTableKey])


pickKey :: Y.Entity DataRow -> DT.Text
pickKey (Y.Entity _ (DataRow key _ _)) = key


mkLearnDatumKey :: ViewId -> DataRowId -> UserId -> Y.Unique LearnDatum
mkLearnDatumKey = LearnItem


mkLearnDatum :: ViewId -> Y.Key DataRow -> UserId -> Int8 -> UTCTime -> LearnDatum
mkLearnDatum = LearnDatum


deleteItems :: forall (m :: * -> *). Y.MonadIO m => [DT.Text] -> ReaderT (Y.PersistEntityBackend DataRow) m [()]
deleteItems deletees = sequence $ map (\portion -> deleteCascadeWhere [DataRowTableKey <-. portion]) $ split 10 deletees


-- | return list of views that refer to the given data source
viewsOnDataSource :: Y.Key DataSource -> PersistResult [Y.Key View]
viewsOnDataSource sourceId = map entityKey `fmap` selectList [ ViewDataSourceId ==. sourceId ] []
