{-|
Description: Users file;  CRUD
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

module Authorisation (upgradeDB, User, persistAction, PerstQ.SqlBackend) where

import qualified Database.Persist.Sqlite as PerstQ
import qualified Yesod as Y
import qualified Yesod.Auth.Account as YAA
import qualified Data.ByteString as DB
import qualified TextItem (TextItem, nullText)
import qualified Control.Monad.Trans.Resource as CMTS (ResourceT)
import qualified Control.Monad.Logger as CML (NoLoggingT)
import qualified Control.Monad.Trans.Reader as CMTR (ReaderT)


Y.share [Y.mkPersist Y.sqlSettings, Y.mkSave "entityDefs"] [Y.persistLowerCase|
User
	username TextItem.TextItem
	password DB.ByteString
	emailAddress TextItem.TextItem
	verified Bool
	verifyKey TextItem.TextItem
	resetPasswordKey TextItem.TextItem
	UniqueUsername username
	deriving Show
|]


instance YAA.PersistUserCredentials User where
	userUsernameF = UserUsername
	-- userPasswordHashF (Y.EntityField val obj) = UserPassword (PerstS.EntityField val (TextItem.fromByteS obj))
	userPasswordHashF = UserPassword
	userEmailF = UserEmailAddress
	userEmailVerifiedF = UserVerified
	userEmailVerifyKeyF = UserVerifyKey
	userResetPwdKeyF = UserResetPasswordKey
	uniqueUsername = UniqueUsername

	userCreate name email key pwd = User name pwd email False key TextItem.nullText


-- | pass in a database action and run it on the users table
persistAction :: (Y.MonadBaseControl IO m, Y.MonadIO m, Y.MonadLogger m) => PerstQ.SqlPersistT m a -> TextItem.TextItem -> m a
persistAction action table = PerstQ.withSqliteConn table enaction where
	enaction = PerstQ.runSqlConn action


upgradeDB :: TextItem.TextItem -> IO ()
upgradeDB table = PerstQ.runSqlite table runrunrun


runrunrun :: CMTR.ReaderT PerstQ.SqlBackend (CML.NoLoggingT (CMTS.ResourceT IO)) ()
runrunrun = PerstQ.runMigration $ PerstQ.migrate entityDefs $ PerstQ.entityDef (Nothing :: Maybe User)
