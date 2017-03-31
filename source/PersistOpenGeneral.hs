{-|
Description: Return a connection pool dependent on the name argument.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

If you are looking for an industry-standard way to specify a general backend string, look away.
This source detects a Postgres prefix only.
-}


{-# LANGUAGE FlexibleContexts #-}


module PersistOpenGeneral (createSqlAnyPool) where


import qualified Data.Text as DT (pack, map, Text, stripPrefix)
import Database.Persist.Sqlite (createSqlitePool)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)


-- | Create and return a back end pool of the type specified in the @connStr@ argument.
createSqlAnyPool :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => DT.Text -> Int -> m ConnectionPool
createSqlAnyPool connStr count = maybe
		(createSqlitePool connStr count)
		(\rest -> createPostgresqlPool (encodeUtf8 $ DT.map semiToSpace rest) count)
		(DT.stripPrefix prefixPg connStr)

		
semiToSpace :: Char -> Char
semiToSpace ';' = ' '
semiToSpace c = c


prefixPg :: DT.Text
prefixPg = DT.pack "dbi:Pg:"
