{-|
Description: Send emails for new-user and password reminder
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}


module EmailVerification (newAccountEmail, resetAccountEmail) where


import qualified Yesod.Core as YC
import qualified Yesod.Core.Handler as YH
import qualified Data.Text as DT (Text, append, concat)
import qualified Network.Mail.Mime as Mime
import qualified Data.Text.Lazy as DTL
import JRState (JRState(..))
import Control.Monad.Logger (logInfoN)
import qualified Network.Wai as WAI
import qualified Branding (visibleName)
import Data.Text.Encoding (decodeUtf8)
import TextShow (showt)


-- | Email full URL to user;  first try to get the host from the request;  if that fails, fall back to configuration data.
emailEnaction :: (YC.MonadLogger m, YC.MonadHandler m, YC.HandlerSite m ~ JRState) => DT.Text -> DT.Text -> DT.Text -> DT.Text -> m ()
emailEnaction what uname email path = YH.waiRequest >>= \req -> maybe (YC.getYesod >>= enactSc) (\hostName -> enactRq (decodeUtf8 hostName) (WAI.isSecure req)) (WAI.requestHeaderHost req) where
	enactRq serverHost isSecure = emailAction what uname email fullURL where
		fullURL = DT.concat [if isSecure then "https" else "http", "://", serverHost, path]
	enactSc site = emailAction what uname email fullURL where
		fullURL = DT.concat [appRoot site, maybe "" (DT.append ":" . showt) (portNumber site), path]


newAccountEmail, resetAccountEmail :: (YC.MonadLogger m, YC.MonadHandler m, YC.HandlerSite m ~ JRState) => DT.Text -> DT.Text -> DT.Text -> m ()

-- | Send email containing verification link for a new account.
newAccountEmail = emailEnaction "Verification"

-- | Send email containing verification link for a password reset.
resetAccountEmail = emailEnaction "Reset password"


emailAction :: (YC.MonadLogger m, YC.MonadIO m) => DT.Text -> DT.Text -> DT.Text -> DT.Text -> m ()
emailAction text uname email url = YC.liftIO (Mime.renderSendMail $ makeEmail uname email url) >> logAccountEmail uname email url text


logAccountEmail :: YC.MonadLogger m => DT.Text -> DT.Text -> DT.Text -> DT.Text -> m ()
logAccountEmail uname email url action = logInfoN $ DT.concat [ action, " email for ", uname, " (", email, "): ", url ]


-- | http://nginx.com/resources/admin-guide/reverse-proxy/ section "Passing Request Headers" for more details;
-- we need this to get the external URL of the service.
makeEmail :: DT.Text -> DT.Text -> DT.Text -> Mime.Mail
makeEmail uname email url =
	Mime.simpleMail'
		(Mime.Address (Just uname) email)
		(Mime.Address (Just $ DT.append Branding.visibleName " Verification") "root@localhost")
		(DT.append Branding.visibleName " Account")
		(DTL.concat [ "Your URL is ", DTL.fromChunks [ url ] ])
