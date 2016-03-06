{-|
Description: 'Guard' next action with a check that the user is authorised.
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module LoginPlease (onlyIfAuthorised) where


import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation
import Data.Text (Text)


onlyIfAuthorised :: (Text -> Foundation.Handler YC.Html) -> Foundation.Handler YC.Html
onlyIfAuthorised action = YA.maybeAuthId >>= maybe loginPlease action


loginPlease :: YC.HandlerT Foundation.JRState IO a
loginPlease = YC.redirect (Foundation.AuthR YA.LoginR)
