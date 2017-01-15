{-|
Description: Data related to a 'live' data-source connection.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module ConnectionData where


import Data.Text (Text)
import Database.HDBC.PostgreSQL (Connection)
import LearningData (DataSourceId)


data DataDescriptor = DataDescriptor {
	columnNames :: [Text],
	sourceId :: DataSourceId,
	openConnection :: DataHandle	-- the connection is specified by the DataSourceId, but this is the opened handle
}


data DataHandle
	= Postgres Connection Text
	| Sqlite3 { tableName :: Text }
	| ExternalCached
