{-|
Description: Execute a SQL statement and do something to the result.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

module ExecuteSqlStmt (exeStmt) where


import Database.HDBC (execute, sFetchAllRows, prepare, toSql)
import Database.HDBC.PostgreSQL (Connection)


-- | A conversion function, connection, a command and a list of substitution parameters.
-- We *always* do *something* with the result, so we might as well put the conversion here.
-- It generally makes the calling site much tidier.
exeStmt :: ([[Maybe String]] -> a) -> Connection -> String -> [String] -> IO a
-- TODO actually look at the Integer returned and throw an error if necessary
exeStmt mash conn command substs = prepare conn command >>= \stmt -> execute stmt (map toSql substs) >> fmap mash (sFetchAllRows stmt)
