{-|
Description: Post-login processing:  load account name and credentials into session
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module LoginPost (getLoginR) where


import qualified Yesod.Auth as YA
import qualified Yesod.Core as YC
import qualified Foundation (Handler)
import qualified JRState
import Data.Text (Text)
import qualified AuthoriStyle (Style(..))
import GoHome (goHome)



getLoginR :: Text -> Foundation.Handler YC.Html
getLoginR acctName = YC.getYesod >>= setAcctIfTrusted acctName


setAcctIfTrusted :: Text -> JRState.JRState -> Foundation.Handler YC.Html
setAcctIfTrusted acctName site = setAcctIfTrusted' acctName (JRState.howAuthorised site)


setAcctIfTrusted' :: Text -> AuthoriStyle.Style -> Foundation.Handler YC.Html
setAcctIfTrusted' acctName AuthoriStyle.Email = goHome
setAcctIfTrusted' acctName AuthoriStyle.Trust = load acctName >> goHome


-- Given a logged-in user name, load its config data into the session
load :: YC.MonadHandler m => Text -> m ()
load acctName = YC.setSession YA.credsKey acctName
