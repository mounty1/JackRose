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


type NewThrottle = Maybe Int


data UserSchemaCpt =
	SubSchema NewThrottle Bool Text UserSchema |
	View {
		dataSource :: DataDescriptor,
		throttle :: NewThrottle,
		shuffle :: Bool,
		label :: Text,
		obverse :: [Node],
		backside :: [Node]
	}


type UserSchema = [UserSchemaCpt]
