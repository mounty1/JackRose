{-|
Description: Data related to a 'live' data-source connection.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module ConnectionSpec where


import Data.Text (Text)
import Database.HDBC.PostgreSQL (Connection)


data DataDescriptor = DataDescriptor {
	columnNames :: [Text],
	primaryKeys :: [Text],
	openConnection :: DataHandle	-- the connection is specified by the DataSourceId, but this is the opened handle
}


data DataHandle
	= Postgres Connection Text
	| Sqlite3 { tableName :: Text }
	| ExternalCached


data PostgresConnection = PostgresConnection {
	serverName :: Text,
	maybePort :: Maybe Int,
	maybeDBName :: Maybe Text,
	userName :: Text,
	maybePassword :: Maybe Text
} deriving (Eq, Ord)
