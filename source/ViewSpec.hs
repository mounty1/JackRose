{-|
Description: Data related to a 'live' data view.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module ViewSpec where


import Data.Text (Text)
import ConnectionSpec (DataDescriptor)
import LearningData (ViewId)


data View = View {
	id :: ViewId,
	name :: Text,
	openConnection :: DataDescriptor
}
