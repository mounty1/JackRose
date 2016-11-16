{-|
Description: Data Declarations of User 'decks'.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module UserDeck where


import Data.Text (Text)
import Text.XML (Node)
import ConnectionData (DataDescriptor)


type NewThrottle = Maybe Int


data UserDeckCpt =
	SubDeck {
		throttle :: NewThrottle,
		shuffle :: Bool,
		label :: Text,
		items :: [UserDeckCpt]
	} |
	TableView {
		throttle :: NewThrottle,
		shuffle :: Bool,
		vid :: Text, -- ^ view identifier
		dataSource :: DataDescriptor,
		obverse :: [Node],
		backside :: [Node]
	}
