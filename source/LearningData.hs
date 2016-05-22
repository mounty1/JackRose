{-|
Description: Management of persistent storage;  items learning, review and scoring data.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

DataSource: reference to some database table or flat file.
DataRow:  in Anki parlance, a note.
View: 'card':  display information * data row
LearnDatum:  card * user.
History:  record of item scores.
-}


{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving #-}


module LearningData (migrateData, DataSourceId, DataSource(..), DataRow(..), View(..), LearnDatum(..), History(..)) where


import qualified Yesod as Y
import qualified Data.Text as DT (Text)
import Authorisation (UserId)
import Data.Int (Int8)
import Data.Time (UTCTime)


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
	key DT.Text NOT NULL
	dataSourceRowId DataSourceId NOT NULL
	loaded UTCTime NOT NULL
	UniqueRowIndex key dataSourceRowId
View
	viewUID DT.Text NOT NULL
	sourceId DataSourceId NOT NULL
	obverse DT.Text NOT NULL
	reverse DT.Text NOT NULL
	UniqueViewIndex viewUID sourceId
LearnDatum
	viewUID ViewId NOT NULL
	user UserId NOT NULL
	nextReview UTCTime NOT NULL
	UniqueLearnDatumIndex viewUID user
History
	item LearnDatumId NOT NULL
	stamp UTCTime NOT NULL
	grade Int8 NOT NULL
|]
