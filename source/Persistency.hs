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


-- {-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}


module Persistency (persistAction, upgradeDB) where


import qualified Database.Persist.Sqlite as PerstQ
import qualified Yesod as Y
import qualified Data.Text as DT (Text)


-- | pass in a database action and run it on the users table
persistAction :: (Y.MonadBaseControl IO m, Y.MonadIO m, Y.MonadLogger m) => PerstQ.SqlPersistT m a -> DT.Text -> m a
persistAction action table = PerstQ.withSqliteConn table enaction where
	enaction = PerstQ.runSqlConn action


upgradeDB :: (Y.MonadBaseControl IO m, Y.MonadIO m) => DT.Text -> PerstQ.Migration -> m ()
upgradeDB table migration = PerstQ.runSqlite table (PerstQ.runMigration migration)
