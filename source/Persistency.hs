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


{-# LANGUAGE FlexibleContexts #-}


module Persistency (upgradeDB) where


import qualified Database.Persist.Sqlite as PerstQ
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)


-- | run upgrade action on a database
upgradeDB :: (MonadBaseControl IO m, MonadIO m) => PerstQ.ConnectionPool -> PerstQ.Migration -> m ()
upgradeDB pool migration = PerstQ.runSqlPool (PerstQ.runMigration migration) pool
