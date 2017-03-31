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
import LearningData (ViewId)


-- | An optional /throttle/ value to limit the daily quota of new items offered for memorisation.
type NewThrottle = Maybe Int


-- | One node (terminal or not) in the hierarchy of views for each user.
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
		vid :: ViewId
	}
