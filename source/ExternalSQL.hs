{-|
Description: Compose and execute a SQL statement.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

module ExternalSQL (get) where


import qualified Data.List as DL (intercalate)
import TextList (deSerialise)
import ExecuteSqlStmt (exeStmt)
import ConnectionSpec (DataHandle(..))
import qualified Data.Text as DT (Text, unpack)


-- | Compose and execute a SQL SELECT statement, to obtain the data-row with the specified primary key.
get :: DT.Text -> [DT.Text] -> DataHandle -> IO [[Maybe String]]
get key keys1y (Postgres conn table) = exeStmt conn ("SELECT * FROM \"" ++ DT.unpack table ++ "\" WHERE " ++ mkSqlWhereClause keys1y ++ ";") (map DT.unpack $ deSerialise key)


mkSqlWhereClause :: [DT.Text] -> String
mkSqlWhereClause keysList = DL.intercalate " AND " (map (\key -> "(\"" ++ DT.unpack key ++ "\"=?)") keysList)
