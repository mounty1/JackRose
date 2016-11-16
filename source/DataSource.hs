{-|
Description: Management of user-defined data sources.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Data sources are the content of question/answer pairs.  They can be SQL tables, CSV flat files etc.
-}


{-# LANGUAGE OverloadedStrings #-}


module DataSource (DataSource(..), DataVariant(..), enSerialise, deSerialise) where


import TextShow (showt)
import qualified Data.Text as DT (Text, singleton, empty, null, head, length)
import qualified TextList as DL (enSerialise, deSerialise)
import MaybeIntValue (maybeIntValue)


data DataSource = DataSource DT.Text DT.Text DataVariant

-- | All the details to identify a data source without actually opening a connection to it.
data DataVariant
	= Postgres { server :: DT.Text, port :: Maybe Int, database :: DT.Text, namespace :: Maybe DT.Text, table :: DT.Text, username :: Maybe DT.Text, password :: Maybe DT.Text }
	| Sqlite3 { tableName :: DT.Text }
	| CSV { separator :: Char, fileCSV :: DT.Text }
	| XMLSource { fileXML :: DT.Text }


showMI :: Maybe Int -> DT.Text
showMI = maybe DT.empty showt


showMT :: Maybe DT.Text -> DT.Text
showMT = maybe DT.empty id


enSerialise :: DataVariant -> DT.Text
enSerialise (Postgres serverIP portNo dbase nameSpace dtable userName passWord) = DL.enSerialise [ "P", serverIP, showMI portNo, dbase, showMT nameSpace, dtable, showMT userName, showMT passWord ]
enSerialise (Sqlite3 dtableName) = DL.enSerialise [ "Q", dtableName ]
enSerialise (CSV recseparator ffileCSV) = DL.enSerialise [ "C", DT.singleton recseparator, ffileCSV ]
enSerialise (XMLSource ffileXML) = DL.enSerialise [ "X", ffileXML ]


deSerialise :: DT.Text -> Maybe DataVariant
deSerialise = deSerialise' . DL.deSerialise


deSerialise' :: [DT.Text] -> Maybe DataVariant
deSerialise' [ "P", serverIP, maybePort, dbase, maybeNamespace, dtable, maybeUsername, maybePassword ] =
		Just $ Postgres
			serverIP
			(maybeIntValue maybePort)
			dbase
			(if DT.null maybeNamespace then Nothing else Just maybeNamespace)
			dtable
			(if DT.null maybeUsername then Nothing else Just maybeUsername)
			(if DT.null maybePassword then Nothing else Just maybePassword)
deSerialise' [ "Q", dtableName ] = Just $ Sqlite3 dtableName
deSerialise' [ "C", recseparator, ffileCSV ] =
		if DT.length recseparator == 1 then
			Just $ CSV (DT.head recseparator) ffileCSV
		else
			Nothing
deSerialise' [ "X", ffileXML ] = Just $ XMLSource ffileXML
deSerialise' _ = Nothing
