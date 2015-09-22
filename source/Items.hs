{-|
Description: Items file;  CRUD
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE GADTs,GeneralizedNewtypeDeriving, FlexibleContexts  #-}

module Items (upgradeDB, Item, persistAction) where

import qualified Database.Persist.Sqlite as PerstQ
import qualified Yesod as Y
import qualified Authorisation (UserId)
import qualified Data.Text as DT (Text)
import qualified Data.Int as DI (Int64)
import qualified Control.Monad.Trans.Resource as CMTS (ResourceT)
import qualified Control.Monad.Logger as CML (NoLoggingT)
import qualified Control.Monad.Trans.Reader as CMTR (ReaderT)


Y.share [Y.mkPersist Y.sqlSettings, Y.mkSave "entityDefs"] [Y.persistLowerCase|
Item
	table DT.Text
	row DT.Text
	view DT.Text
	identity Authorisation.UserId
	itemNo DI.Int64
	deriving Show
|]


-- | pass in a database action and run it on the users table
persistAction :: (Y.MonadBaseControl IO m, Y.MonadIO m, Y.MonadLogger m) => PerstQ.SqlPersistT m a -> DT.Text -> m a
persistAction action table = PerstQ.withSqliteConn table enaction where
	enaction = PerstQ.runSqlConn action


upgradeDB :: DT.Text -> IO ()
upgradeDB table = PerstQ.runSqlite table runrunrun


runrunrun :: CMTR.ReaderT PerstQ.SqlBackend (CML.NoLoggingT (CMTS.ResourceT IO)) ()
runrunrun = PerstQ.runMigration $ PerstQ.migrate entityDefs $ PerstQ.entityDef (Nothing :: Maybe Item)
