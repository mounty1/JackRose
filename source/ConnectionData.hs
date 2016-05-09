{-|
Description: Data related to a 'live' data-source connection.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module ConnectionData where


import Data.Text (Text)
import Database.HSQL (Connection)


data DataDescriptor = DataDescriptor Text [Text] DataHandle


data DataHandle
        = Postgres Connection Text
        | Sqlite3 { tableName :: Text }
        | CSV { separator :: Char, fileCSV :: Text }
        | XMLSource { fileXML :: Text }
