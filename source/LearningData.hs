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
		getView,
		mkDataRowKey,
		getDataRowKey,
		mkLearnDatumKey,
		getLearnDatumKey) where


import qualified Yesod as Y
import qualified Data.Text as DT (Text)
import Authorisation (UserId)
import Data.Int (Int8)
import Data.Time (UTCTime)
import Database.Persist (selectList, (<.), (==.), (<-.), SelectOpt(LimitTo) )
import qualified Database.Persist.Sql (SqlBackend)
import qualified Control.Monad.Trans.Reader (ReaderT)


Y.share [Y.mkPersist Y.sqlSettings, Y.mkMigrate "migrateData"] [Y.persistLowerCase|
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
	Primary tableKey dataSourceRowId
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
	Primary viewUID itemId user
History
	item LearnDatumId NOT NULL
	stamp UTCTime NOT NULL
	grade Int8 NOT NULL
	Primary item stamp
|]

{- Activity:
	0 new item
	1 suspended
	2 active
	3 suppressed by selection criteria
-}

nextItem :: forall (m :: * -> *). Y.MonadIO m => Int8 -> [Y.Filter LearnDatum] -> UserId -> [Y.Key View] -> Control.Monad.Trans.Reader.ReaderT Database.Persist.Sql.SqlBackend m [Y.Entity LearnDatum]
nextItem activityState extras user views = selectList ((LearnDatumActivity ==. activityState) : (LearnDatumUser ==. user) : (LearnDatumViewUID <-. views) : extras) [ LimitTo 1 ]


newItem :: forall (m :: * -> *). Y.MonadIO m => UserId -> [Y.Key View] -> Control.Monad.Trans.Reader.ReaderT Database.Persist.Sql.SqlBackend m [Y.Entity LearnDatum]
newItem user views = nextItem 0 [] user views


dueItem :: forall (m :: * -> *). Y.MonadIO m => UserId -> UTCTime -> [Y.Key View] -> Control.Monad.Trans.Reader.ReaderT Database.Persist.Sql.SqlBackend m [Y.Entity LearnDatum]
dueItem user stamp views = nextItem 2 [ LearnDatumNextReview <. stamp ] user views


getDataRow :: forall (m :: * -> *). Y.MonadIO m => DataRowId -> Control.Monad.Trans.Reader.ReaderT (Y.PersistEntityBackend DataRow) m (Maybe DataRow)
getDataRow = Y.get


getView :: forall (m :: * -> *). Y.MonadIO m => ViewId -> Control.Monad.Trans.Reader.ReaderT (Y.PersistEntityBackend View) m (Maybe View)
getView = Y.get


mkDataRowKey :: DT.Text -> DataSourceId -> Y.Key DataRow
mkDataRowKey = DataRowKey


getDataRowKey :: Y.Key DataRow -> (DT.Text, DataSourceId)
getDataRowKey (DataRowKey k d) = (k, d)


mkLearnDatumKey :: ViewId -> DataRowId -> UserId -> Y.Key LearnDatum
mkLearnDatumKey = LearnDatumKey


getLearnDatumKey :: Y.Key LearnDatum -> (ViewId, DataRowId, UserId)
getLearnDatumKey (LearnDatumKey v i u) = (v, i, u)
