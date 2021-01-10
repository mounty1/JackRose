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


module SessionItemData (set, get, Bundle(..)) where


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

-- | 'Bundle' -- the data preserved in the client-side session.
newtype Bundle = Bundle (Key LearnDatum)


set :: MonadHandler m => Key LearnDatum -> m ()
-- | Put the item data into the session
set = setSession itemKey . showt . fromSqlKey


get :: MonadHandler m => m (Maybe Bundle)
-- | Get the item data.
-- The data are then removed from the session; they can only be gotten once.
-- If they will be needed later, they must be re-set.
get = fmap (fmap toKey . (maybeIntValue =<<)) (lookupSession itemKey) >>= (>>) (deleteSession itemKey) . return


toKey :: Int64 -> Bundle
toKey = Bundle . toSqlKey
