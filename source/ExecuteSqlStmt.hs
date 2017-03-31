{-|
Description: Execute a SQL statement.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

module ExecuteSqlStmt (exeStmt) where


import Database.HDBC (execute, sFetchAllRows, prepare, toSql)
import Database.HDBC.PostgreSQL (Connection)


-- | A connection, a command and a list of substitution parameters.
exeStmt :: Connection -> String -> [String] -> IO [[Maybe String]]
-- TODO actually look at the Integer returned and throw an error if necessary
exeStmt conn command substs = prepare conn command >>= \stmt -> execute stmt (map toSql substs) >> sFetchAllRows stmt
