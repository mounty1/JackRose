{-|
Description: Management of persistent storage;  hierarchical deck view per user.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

A user has a tree-structured hierarchy of views which specify the order in which
cards are presented for review;  hence the usage of terms _node_ and _end_ in
this source.
-}


{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, RankNTypes, GeneralizedNewtypeDeriving, FlexibleInstances, DeriveGeneric #-}
{-# OPTIONS_HADDOCK prune #-}


module DeckData (migrateData, userDeckNodes, userDeckEnds, userDeckEndsViewed, UserDeckNode(..), UserDeckEnd(..), UserDeckNodeId) where


import qualified Yesod as Y
import qualified Data.Text as DT (Text)
import Authorisation (UserId)
import LearningData (ViewId)
import Database.Persist (selectList, (==.), (<-.))
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


-- | Return a list of all deck nodes for the given user.
userDeckNodes :: forall (m :: * -> *). Y.MonadIO m => UserId -> Control.Monad.Trans.Reader.ReaderT (Y.PersistEntityBackend UserDeckNode) m [Y.Entity UserDeckNode]
userDeckNodes user = selectList [ UserDeckNodeUser ==. user ] []


-- | Return a list of all deck ends for the given user.
userDeckEnds :: forall (m :: * -> *). Y.MonadIO m => UserId -> Control.Monad.Trans.Reader.ReaderT (Y.PersistEntityBackend UserDeckEnd) m [Y.Entity UserDeckEnd]
userDeckEnds user = selectList [ UserDeckEndUser ==. user ] []


-- | Return a list of all deck nodes for the given user, that refer to one of the given list of view.
userDeckEndsViewed :: forall (m :: * -> *). Y.MonadIO m => UserId -> [ViewId] -> Control.Monad.Trans.Reader.ReaderT (Y.PersistEntityBackend UserDeckEnd) m [Y.Entity UserDeckEnd]
-- TODO:  split this up as per LearningData.deleteItems
userDeckEndsViewed user views = selectList [ UserDeckEndUser ==. user, UserDeckEndViewId <-. views ] []
