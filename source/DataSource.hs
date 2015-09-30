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


module DataSource (DataSource(..), DataVariant(..)) where


import qualified Data.Text as DT (Text, concat)


data DataSource = DataSource DT.Text DT.Text DataVariant


data DataVariant
		= Postgres { server :: DT.Text, port :: Int, database :: DT.Text, namespace :: Maybe DT.Text, table :: DT.Text }
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