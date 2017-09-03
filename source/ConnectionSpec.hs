{-|
Description: Data related to a 'live' data-source connection.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module ConnectionSpec (DataHandle(Postgres), DataDescriptor(..), PostgresConnection(..)) where


import Data.Text (Text)
import Database.HDBC.PostgreSQL (Connection)


-- | In-memory representation of one external data source, with
-- column names, which columns are the primary keys, and the open handle.
data DataDescriptor = DataDescriptor {
	columnNames :: [Text],
	primaryKeys :: [Text],
	openConnection :: DataHandle	-- the connection is specified by the DataSourceId, but this is the opened handle
}


-- | An open connection or handle to an external data source.
data DataHandle
	= Postgres Connection Text
	| Sqlite3 Text
	| ExternalCached


-- | Pooled Postgres connection object;  see 'JRState' for usage.
data PostgresConnection = PostgresConnection {
	serverName :: Text,
	maybePort :: Maybe Int,
	maybeDBName :: Maybe Text,
	userName :: Text,
	maybePassword :: Maybe Text
} deriving (Eq, Ord)
