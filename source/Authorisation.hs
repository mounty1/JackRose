{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeSynonymInstances #-}

module Authorisation (upgradeDB, User, persistAction, PerstQ.SqlBackend) where

import qualified Database.Persist.Sqlite as PerstQ
import qualified Yesod as Y
import qualified Yesod.Auth.Account as YAA
import qualified Foundation (authTable, JRState)
import qualified Data.ByteString as DB
import qualified Pervasive (TextItem, nullText)
import qualified Control.Monad.Trans.Resource as CMTS (ResourceT)
import qualified Control.Monad.Logger as CML (NoLoggingT)
import qualified Control.Monad.Trans.Reader as CMTR (ReaderT)


Y.share [Y.mkPersist Y.sqlSettings, Y.mkSave "entityDefs"] [Y.persistLowerCase|
User
	username Pervasive.TextItem
	password DB.ByteString
	emailAddress Pervasive.TextItem
	verified Bool
	verifyKey Pervasive.TextItem
	resetPasswordKey Pervasive.TextItem
	UniqueUsername username
	deriving Show
|]


instance YAA.PersistUserCredentials User where
	userUsernameF = UserUsername
	-- userPasswordHashF (Y.EntityField val obj) = UserPassword (PerstS.EntityField val (Pervasive.fromByteS obj))
	userPasswordHashF = UserPassword
	userEmailF = UserEmailAddress
	userEmailVerifiedF = UserVerified
	userEmailVerifyKeyF = UserVerifyKey
	userResetPwdKeyF = UserResetPasswordKey
	uniqueUsername = UniqueUsername

	userCreate name email key pwd = User name pwd email False key Pervasive.nullText

	
persistAction :: (Y.MonadBaseControl IO m, Y.MonadIO m, Y.MonadLogger m) => PerstQ.SqlPersistT m a -> Foundation.JRState -> m a
persistAction action site = PerstQ.withSqliteConn (Foundation.authTable site) enaction where
	enaction = PerstQ.runSqlConn action


upgradeDB :: Foundation.JRState -> IO ()
upgradeDB site = PerstQ.runSqlite (Foundation.authTable site) runrunrun


runrunrun :: CMTR.ReaderT PerstQ.SqlBackend (CML.NoLoggingT (CMTS.ResourceT IO)) ()
runrunrun = PerstQ.runMigration $ PerstQ.migrate entityDefs $ PerstQ.entityDef (Nothing :: Maybe User)
