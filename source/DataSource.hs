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


import TextShow (showt)
import qualified Data.Text as DT (splitOn, Text, singleton, concat, empty, null, isInfixOf, head, length)
import qualified Data.Text.Read as DTR (decimal)


data DataSource = DataSource DT.Text DT.Text DataVariant


data DataVariant
        = Postgres { server :: DT.Text, port :: Maybe Int, database :: DT.Text, namespace :: Maybe DT.Text, table :: DT.Text }
        | Sqlite3 { tableName :: DT.Text }
        | CSV { separator :: Char, fileCSV :: DT.Text }
        | XMLSource { fileXML :: DT.Text }


showMI :: Maybe Int -> DT.Text
showMI Nothing = DT.empty
showMI (Just n) = showt n


showMT :: Maybe DT.Text -> DT.Text
showMT Nothing = DT.empty
showMT (Just text) = text


fs :: DT.Text
fs = "/"


maybeIntValue :: DT.Text -> Maybe Int
maybeIntValue = reduceIt . DTR.decimal


reduceIt :: Either String (Int, DT.Text) -> Maybe Int
reduceIt (Right (n, "")) = Just n
reduceIt _ = Nothing



enSerialise :: DataVariant -> DT.Text
enSerialise (Postgres server port database namespace table) = DT.concat [ "P", fs, server, fs, showMI port, fs, database, fs, showMT namespace, fs, table ]
enSerialise (Sqlite3 tableName) = DT.concat [ "Q", fs, tableName ]
enSerialise (CSV separator fileCSV) = DT.concat [ "C", fs, DT.singleton separator, fs, fileCSV ]
enSerialise (XMLSource fileXML) = DT.concat [ "X", fs, fileXML ]


excludesFS :: DT.Text -> Bool
excludesFS = not . DT.isInfixOf fs


deSerialise :: DT.Text -> Maybe DataVariant
deSerialise = deSerialise' . DT.splitOn fs


deSerialise' :: [DT.Text] -> Maybe DataVariant
deSerialise' [ "P", server, maybePort, database, maybeNamespace, table ] =
		Just $ Postgres
			server
			(maybeIntValue maybePort)
			database
			(if DT.null maybeNamespace then Just maybeNamespace else Nothing)
			table
deSerialise' [ "Q", tableName ]  = Just $ Sqlite3 tableName
deSerialise' [ "C", separator, fileCSV ] =
		if DT.length separator == 1 then
			Just $ CSV (DT.head separator) fileCSV
		else
			Nothing
deSerialise' [ "X", fileXML ]  = Just $ XMLSource fileXML
deSerialise' _ = Nothing
