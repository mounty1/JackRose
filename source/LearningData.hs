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
		dueItems,
		History(..),
		getDataRow,
		mkViewKey,
		getViewKey,
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
	shortName DT.Text UNIQUE NOT NULL
	longName DT.Text
	sourceSerial DT.Text UNIQUE NOT NULL
	resynced UTCTime NOT NULL
	ByName shortName
DataRow
	tableKey DT.Text NOT NULL
	dataSourceRowId DataSourceId NOT NULL
	loaded UTCTime NOT NULL
	Primary tableKey dataSourceRowId
View
	uid DT.Text NOT NULL
	dataSourceId DataSourceId NOT NULL
	obverse DT.Text NOT NULL
	reverse DT.Text NOT NULL
	Primary uid
LearnDatum
	viewUID ViewId NOT NULL
	itemId DataRowId NOT NULL
	user UserId NOT NULL
	nextReview UTCTime NOT NULL
	Primary viewUID itemId user
History
	item LearnDatumId NOT NULL
	stamp UTCTime NOT NULL
	grade Int8 NOT NULL
	Primary item stamp
|]

{- if Views are not associated with DataSources, how do we know which Views go with which DataSources ? -}

dueItems :: forall (m :: * -> *). Y.MonadIO m => UserId -> UTCTime -> [Y.Key View] -> Control.Monad.Trans.Reader.ReaderT Database.Persist.Sql.SqlBackend m [Y.Entity LearnDatum]
dueItems user stamp views = selectList [ LearnDatumNextReview <. stamp, LearnDatumUser ==. user, LearnDatumViewUID <-. views ] [ LimitTo 1 ]


getDataRow :: Y.MonadIO m => DataRowId -> Control.Monad.Trans.Reader.ReaderT (Y.PersistEntityBackend DataRow) m (Maybe DataRow)
getDataRow dataRowId = Y.get dataRowId


mkViewKey :: DT.Text -> Y.Key View
mkViewKey item = ViewKey { unViewKey = item }


getViewKey :: Y.Key View -> DT.Text
getViewKey item = unViewKey item


mkDataRowKey :: DT.Text -> DataSourceId -> Y.Key DataRow
mkDataRowKey k d = DataRowKey k d


getDataRowKey :: Y.Key DataRow -> (DT.Text, DataSourceId)
getDataRowKey (DataRowKey k d) = (k, d)


mkLearnDatumKey :: ViewId -> DataRowId -> UserId -> Y.Key LearnDatum
mkLearnDatumKey v i u = LearnDatumKey v i u


getLearnDatumKey :: Y.Key LearnDatum -> (ViewId, DataRowId, UserId)
getLearnDatumKey (LearnDatumKey v i u) = (v, i, u)
