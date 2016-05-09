{-|
Description: Obtain a 'live' 'handle' to a data source;  i.e., open it.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module DataConnection (DataDescriptor, DataHandle) where


import qualified Data.Text as DT (Text, singleton, empty, null, head, length)
import qualified Data.Text.Read as DTR (decimal)
import qualified DataSource (DataSource(..))
import qualified Database.Persist.Postgresql as DPP
import qualified Database.Persist.Sql as DPQ
import qualified JRState (JRState, runFilteredLoggingT)
import qualified Control.Monad.Logger as CML (runStdoutLoggingT)
import ConnectionData (DataDescriptor(..), DataHandle(..))

{-
data DataVariant
        = Postgres { server :: DT.Text, port :: Maybe Int, database :: DT.Text, namespace :: Maybe DT.Text, table :: DT.Text, username :: Maybe DT.Text, password :: Maybe DT.Text }
        | Sqlite3 { tableName :: DT.Text }
        | CSV { separator :: Char, fileCSV :: DT.Text }
        | XMLSource { fileXML :: DT.Text }
-}

openConnection :: DataSource.DataSource -> JRState.JRState -> DataDescriptor
openConnection site (DataSource.DataSource _ longName varPart) = DataDescriptor longName (openConn' site varPart)


openConn' :: DataSource.DataVariant -> JRState.JRState -> IO DataHandle
openConn' site (Postgres server :: DT.Text, port :: Maybe Int, database :: DT.Text, namespace :: Maybe DT.Text, table :: DT.Text, username :: Maybe DT.Text, password :: Maybe DT.Text ) =
		(CML.runStdoutLoggingT $ JRState.runFilteredLoggingT site $ DPP.createPostgresqlPool connString 5) >>= flip . (return . Postgres) table where
		connString = DT.concat ["host=", server, mkIntCpt "port" port, mkConnCpt "user" username, "  dbname=", database, mkConnCpt "password" password]
		mkConnCpt label maybeStr = maybe DT.empty (\c -> DT.concat [" ", label, "=", c])
		mkIntCpt label maybeInt = mkConnCpt label (fmap show maybeInt)
