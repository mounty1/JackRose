{-|
Description: Management of user-defined data sources.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Data sources are the content of question/answer pairs.  They can be SQL tables, CSV flat files etc.
-}


{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module DataSource (DataSource(..), DataVariant(..), upgradeDB) where


import qualified Database.Persist.Sqlite as PerstQ
import qualified Yesod as Y
import qualified Yesod.Auth.Account as YAA
import qualified Data.ByteString as DB
import qualified Data.Text as DT (Text, concat)
import qualified Control.Monad.Trans.Resource as CMTS (ResourceT)
import qualified Control.Monad.Logger as CML (NoLoggingT)
import qualified Control.Monad.Trans.Reader as CMTR (ReaderT)
import Data.Int (Int64)


data DataSource = DataSource DT.Text DT.Text DataVariant


data DataVariant
		= Postgres { server :: DT.Text, port :: Maybe Int, database :: DT.Text, namespace :: Maybe DT.Text, table :: DT.Text }
		| Sqlite3 { tableName :: DT.Text }
		| CSV { separator :: Char, fileCSV :: DT.Text }
		| XMLSource { fileXML :: DT.Text }


serialise :: DataVariant -> DT.Text
serialise (Postgres server port database namespace table) = DT.concat [ "P/" ]
serialise (Sqlite3 tableName) = DT.concat [ "Q/" ]
serialise (CSV separator fileCSV) = DT.concat [ "C/" ]
serialise (XMLSource fileXML) = DT.concat [ "X/" ]

{-
deserialise :: DT.Text -> DataVariant
deserialise ["P"] = Postgres server port database namespace table
deserialise ["Q", tableName] = Sqlite3 tableName
deserialise ["C", separator, fileCSV] = CSV separator fileCSV
deserialise ["X", fileXML] = XMLSource fileXML
-}

{- Three tables:

	DataSourceIndex:  one row per table;  maps table reference to UID
	ViewIndex:  one row per view*datasource:  maps view*datasoure to UID
	RowIndex:  one row per unique_key*viewIndex
-}
Y.share [Y.mkPersist Y.sqlSettings, Y.mkMigrate "sourceFlatten"] [Y.persistLowerCase|
DataSourceIndex
	sourceSerial DT.Text UNIQUE NOT NULL
	sourceIndex Int NOT NULL
	UniqueSourceSerial sourceSerial
ViewIndex
	viewUID DT.Text
	viewIndex Int NOT NULL
	sourceIndex Int NOT NULL
	UniqueViewIndex viewIndex viewUID
RowIndex
	viewIndex Int NOT NULL
	key DT.Text NOT NULL
	rowIndex Int64 NOT NULL
	UniqueRowIndex key viewIndex
|]


-- | pass in a database action and run it on the users table
persistAction :: (Y.MonadBaseControl IO m, Y.MonadIO m, Y.MonadLogger m) => PerstQ.SqlPersistT m a -> DT.Text -> m a
persistAction action table = PerstQ.withSqliteConn table enaction where
	enaction = PerstQ.runSqlConn action


upgradeDB :: DT.Text -> IO ()
upgradeDB table = PerstQ.runSqlite table runrunrun


runrunrun :: CMTR.ReaderT PerstQ.SqlBackend (CML.NoLoggingT (CMTS.ResourceT IO)) ()
runrunrun = PerstQ.runMigration sourceFlatten
