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
import Data.List (intersperse)

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


flatten :: [DT.Text] -> DT.Text
flatten = DT.concat . intersperse fs

enSerialise :: DataVariant -> DT.Text
enSerialise (Postgres serverIP portNo dbase nameSpace dtable) = flatten [ "P", serverIP, showMI portNo, dbase, showMT nameSpace, dtable ]
enSerialise (Sqlite3 dtableName) = flatten [ "Q", dtableName ]
enSerialise (CSV recseparator ffileCSV) = flatten [ "C", DT.singleton recseparator, ffileCSV ]
enSerialise (XMLSource ffileXML) = flatten [ "X", ffileXML ]


excludesFS :: DT.Text -> Bool
excludesFS = not . DT.isInfixOf fs


deSerialise :: DT.Text -> Maybe DataVariant
deSerialise = deSerialise' . DT.splitOn fs


deSerialise' :: [DT.Text] -> Maybe DataVariant
deSerialise' [ "P", serverIP, maybePort, dbase, maybeNamespace, dtable ] =
		Just $ Postgres
			serverIP
			(maybeIntValue maybePort)
			dbase
			(if DT.null maybeNamespace then Just maybeNamespace else Nothing)
			dtable
deSerialise' [ "Q", dtableName ]  = Just $ Sqlite3 dtableName
deSerialise' [ "C", recseparator, ffileCSV ] =
		if DT.length recseparator == 1 then
			Just $ CSV (DT.head recseparator) ffileCSV
		else
			Nothing
deSerialise' [ "X", ffileXML ]  = Just $ XMLSource ffileXML
deSerialise' _ = Nothing
