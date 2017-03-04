{-|
Description: Management of user-defined data sources;  to be repealed.  Only retained for how enSerialise might work
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Data sources are the content of question/answer pairs.  They can be SQL tables, CSV flat files etc.
-}


{-# LANGUAGE OverloadedStrings #-}


module DataSource (enSerialise) where


import TextShow (showt)
import qualified Data.Text as DT (Text, singleton, empty, null, head, length)
import qualified TextList as DL (enSerialise, deSerialise)


-- | All the details to identify a data source without actually opening a connection to it.
data DataVariant
	= Postgres { server :: DT.Text, port :: Maybe Int, database :: Maybe DT.Text, table :: DT.Text, username :: Maybe DT.Text, password :: Maybe DT.Text }
	| Sqlite3 { tableName :: DT.Text }
	| CSV { separator :: Char, fileCSV :: DT.Text }
	| XMLSource { fileXML :: DT.Text }


showMI :: Maybe Int -> DT.Text
showMI = maybe DT.empty showt


showMT :: Maybe DT.Text -> DT.Text
showMT = maybe DT.empty id


-- TODO we don't need this;  replace with something that validates JSON input or similar.
enSerialise :: DataVariant -> DT.Text
enSerialise (Postgres serverIP portNo dbase dtable userName passWord) = DL.enSerialise [ "P", serverIP, showMI portNo, showMT dbase, dtable, showMT userName, showMT passWord ]
enSerialise (Sqlite3 dtableName) = DL.enSerialise [ "Q", dtableName ]
enSerialise (CSV recseparator ffileCSV) = DL.enSerialise [ "C", DT.singleton recseparator, ffileCSV ]
enSerialise (XMLSource ffileXML) = DL.enSerialise [ "X", ffileXML ]
