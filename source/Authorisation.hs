{-|
Description: Users file;  CRUD
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, GADTs, GeneralizedNewtypeDeriving #-}


module Authorisation (migrateData, User, UserId, Member) where


import qualified Yesod as Y
import qualified Yesod.Auth.Account as YAA
import qualified Data.ByteString as DB
import qualified Data.Text as DT (Text, empty)


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
	Membership child parent
|]


instance YAA.PersistUserCredentials User where
	userUsernameF = UserUsername
	userPasswordHashF = UserPassword
	userEmailF = UserEmailAddress
	userEmailVerifiedF = UserVerified
	userEmailVerifyKeyF = UserVerifyKey
	userResetPwdKeyF = UserResetPasswordKey
	uniqueUsername = UniqueUsername
	userCreate name email key pwd = User name pwd email False key DT.empty
