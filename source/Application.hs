{-|
Description: Top-level routing;  exports just the instances in @mkYesodDispatch@
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

To keep the code as clean as possible, have just the bare minimum in this source,
where it is subject to the various LANGUAGE extensions, and perform 'real'
processing where all this black magic isn't in effect.
-}


{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns, TypeFamilies, MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Application () where


import qualified Yesod
import qualified Yesod.Auth as YA
import ReviewGet (getHomeR)
import ReviewPost (postHomeR)
import LoginPost (getLoginR)
import Foundation
import JRState (JRState)


Yesod.mkYesodDispatch "JRState" resourcesJRState
