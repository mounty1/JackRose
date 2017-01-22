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


{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, RankNTypes, GeneralizedNewtypeDeriving, FlexibleInstances, DeriveGeneric #-}


module DeckData (migrateData, userDeckNodes, userDeckEnds, UserDeckNode(..), UserDeckEnd(..), UserDeckNodeId, UserDeckEndId) where


import qualified Yesod as Y
import qualified Data.Text as DT (Text)
import Authorisation (UserId)
import LearningData (ViewId)
import Database.Persist (selectList, (==.))
import qualified Control.Monad.Trans.Reader (ReaderT)


Y.share [Y.mkPersist Y.sqlSettings, Y.mkMigrate "migrateData"] [Y.persistLowerCase|
UserDeckNode
	parent UserDeckNodeId Maybe
	user UserId NOT NULL
	throttle Int Maybe
	shuffle Bool Maybe
	label DT.Text NOT NULL
UserDeckEnd
	viewId ViewId NOT NULL
	user UserId NOT NULL
	parent UserDeckNodeId Maybe
	throttle Int Maybe
	shuffle Bool Maybe
	Primary viewId user
|]


userDeckNodes :: forall (m :: * -> *). Y.MonadIO m => UserId -> Control.Monad.Trans.Reader.ReaderT (Y.PersistEntityBackend UserDeckNode) m [Y.Entity UserDeckNode]
userDeckNodes user = selectList [ UserDeckNodeUser ==. user ] []


userDeckEnds :: forall (m :: * -> *). Y.MonadIO m => UserId -> Control.Monad.Trans.Reader.ReaderT (Y.PersistEntityBackend UserDeckEnd) m [Y.Entity UserDeckEnd]
userDeckEnds user = selectList [ UserDeckEndUser ==. user ] []
