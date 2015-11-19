{-|
Description: Management of persistent storage;  items learning, review and scoring data.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

DataSource: reference to some database table or flat file.
Item:  item within table
View: 'card':  display information + data source

In all cases these are mapped to the SQLite rowid
-}


{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving #-}


module Persistency (upgradeDB) where


import qualified Database.Persist.Sqlite as PerstQ
import qualified Yesod as Y
import qualified Data.Text as DT (Text)
import qualified Control.Monad.Trans.Resource as CMTS (ResourceT)
import qualified Control.Monad.Logger as CML (NoLoggingT)
import qualified Control.Monad.Trans.Reader as CMTR (ReaderT)
import Data.Int (Int64)


Y.share [Y.mkPersist Y.sqlSettings, Y.mkMigrate "sourceFlatten"] [Y.persistLowerCase|
DataSource
	sourceSerial DT.Text UNIQUE NOT NULL
	UniqueSourceSerial sourceSerial
Item
	key DT.Text NOT NULL
	dataSourceRowId Int64 NOT NULL
	UniqueRowIndex key dataSourceRowId
View
	viewUID DT.Text
	itemRowId Int64 NOT NULL
	UniqueViewIndex viewUID itemRowId
|]


-- | pass in a database action and run it on the users table
persistAction :: (Y.MonadBaseControl IO m, Y.MonadIO m, Y.MonadLogger m) => PerstQ.SqlPersistT m a -> DT.Text -> m a
persistAction action table = PerstQ.withSqliteConn table enaction where
	enaction = PerstQ.runSqlConn action


upgradeDB :: DT.Text -> IO ()
upgradeDB table = PerstQ.runSqlite table runrunrun


runrunrun :: CMTR.ReaderT PerstQ.SqlBackend (CML.NoLoggingT (CMTS.ResourceT IO)) ()
runrunrun = PerstQ.runMigration sourceFlatten
