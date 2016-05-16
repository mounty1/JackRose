{-|
Description: Data Declarations of User Schema
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module ConfigData where


import Data.Text (Text)
import Text.XML (Node)
import ConnectionData (DataDescriptor)


data UserSchemaCpt =
	SubSchema Text UserSchema |
	View {
		dataSource :: DataDescriptor,
		label :: Text,
		obverse :: [Node],
		backside :: [Node]
	}


type UserSchema = [UserSchemaCpt]
