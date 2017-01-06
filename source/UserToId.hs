{-|
Description: Users file;  function:  username to id
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE FlexibleContexts, RankNTypes, KindSignatures #-}


module UserToId (userToId) where


import Authorisation (mkUser, User)
import JRState (runFilteredLoggingT, tablesFile, JRState)
import Database.Persist (getBy)
import Database.Persist.Sqlite (runSqlPool)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Yesod (Entity)
import qualified Data.Text as DT (Text)


userToId :: forall (m :: * -> *). (MonadIO m, MonadBaseControl IO m) => JRState -> DT.Text -> m (Maybe (Entity User))
userToId site user = runFilteredLoggingT site (runSqlPool (getBy $ mkUser user) (tablesFile site))
