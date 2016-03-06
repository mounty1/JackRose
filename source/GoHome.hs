{-|
Description: Present current home page, whatever that is
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module GoHome (goHome) where


import qualified Yesod.Core as YC
import qualified Foundation


goHome :: Foundation.Handler YC.Html
goHome = YC.redirect Foundation.HomeR
