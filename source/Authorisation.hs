{-|
Description: Users file;  CRUD
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, GADTs, GeneralizedNewtypeDeriving, RankNTypes #-}


module Authorisation (migrateData, User(..), UserId, mkUser, userList) where


import qualified Yesod as Y
import qualified Yesod.Auth.Account as YAA
import qualified Data.ByteString as DB
import qualified Data.Text as DT (Text, empty)
import Database.Persist (selectList, (==.))
import qualified Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Types (entityKey)


Y.share [Y.mkPersist Y.sqlSettings, Y.mkMigrate "migrateData"] [Y.persistLowerCase|
User
	username DT.Text NOT NULL
	password DB.ByteString
	emailAddress DT.Text
	verified Bool
	verifyKey DT.Text
	resetPasswordKey DT.Text
	UniqueUsername username
Member
	child UserId NOT NULL
	parent UserId NOT NULL
|]


mkUser :: DT.Text -> Y.Unique User
mkUser = UniqueUsername


-- | list of real, verified users.
-- Unverified users are either real users who haven't yet verified their accounts, or, if they have no email,
-- fake 'group' user accounts.
userList :: forall (m :: * -> *). Y.MonadIO m => Control.Monad.Trans.Reader.ReaderT (Y.PersistEntityBackend User) m [UserId]
userList = map entityKey `fmap` selectList [ UserVerified ==. True ] []


instance YAA.PersistUserCredentials User where
	userUsernameF = UserUsername
	userPasswordHashF = UserPassword
	userEmailF = UserEmailAddress
	userEmailVerifiedF = UserVerified
	userEmailVerifyKeyF = UserVerifyKey
	userResetPwdKeyF = UserResetPasswordKey
	uniqueUsername = UniqueUsername
	userCreate name email key pwd = User name pwd email False key DT.empty
