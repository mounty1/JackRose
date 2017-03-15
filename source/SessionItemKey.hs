{-|
Description: Set and Get a LearnDatum key in the user session.
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

We keep this is one place to ensure that what is retrieved is consistent
with what is set, and is returned as the intended type.
-}


module SessionItemKey (set, get) where


import Yesod.Core (MonadHandler)
import Yesod.Core.Handler (setSession, lookupSession, deleteSession)
import qualified Data.Text as DT (Text, pack)
import Database.Persist.Sql (fromSqlKey, toSqlKey, Key)
import TextShow (showt)
import LearningData (LearnDatum)
import MaybeIntValue (maybeIntValue)
import GHC.Int (Int64)


itemKey :: DT.Text
itemKey = DT.pack "JR.item"


type DatumType = Key LearnDatum


-- | Set the key.
set :: MonadHandler m => DatumType -> m ()
set = setSession itemKey . showt . fromSqlKey


-- | Get the key.
-- The key is then removed from the session; it can only be gotten once.
-- If it will be needed later, it must be re-set.
get :: MonadHandler m => m (Maybe DatumType)
get = fmap (\mT -> fmap toKey (mT >>= maybeIntValue)) (lookupSession itemKey) >>= (>>) (deleteSession itemKey) . return


toKey :: Int64 -> DatumType
toKey = toSqlKey