{-|
Description: Given a logged-in user name, load its config data into the session
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

module Login (load) where


import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import Data.Text (Text)


load :: YC.MonadHandler m => Text -> m ()
load acctName = YC.setSession YA.credsKey acctName
