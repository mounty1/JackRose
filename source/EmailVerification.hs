{-|
Description: Send emails for new-user and password reminder
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

module EmailVerification (newAccountEmail, resetAccountEmail) where


import qualified Yesod.Core as YC
import qualified Data.Text as DT (Text, pack, concat)
import qualified Network.Mail.SMTP as SMTP
import qualified Network.Mail.Mime as Mime
import qualified Logging
import qualified Data.Text.Lazy as DTL


newAccountEmail, resetAccountEmail :: (YC.MonadIO m, YC.MonadLogger m) => DT.Text -> DT.Text -> DT.Text -> m ()

newAccountEmail = emailAction "Verification"

resetAccountEmail = emailAction "Reset password"


emailAction :: (YC.MonadLogger m, YC.MonadIO m) => String -> DT.Text -> DT.Text -> DT.Text -> m ()
emailAction text uname email url = do
	YC.liftIO $ SMTP.sendMail "localhost" (makeEmail uname email url)
	logAccountEmail uname email url text


logAccountEmail :: YC.MonadLogger m => DT.Text -> DT.Text -> DT.Text -> String -> m ()
logAccountEmail uname email url action =
	Logging.logInfo $ DT.concat [ DT.pack action, DT.pack " email for ", uname, DT.pack " (", email, DT.pack "): ", url ]


-- | http://nginx.com/resources/admin-guide/reverse-proxy/ section "Passing Request Headers" for more details
-- | we need this to get the external URL of the service.
makeEmail :: DT.Text -> DT.Text -> DT.Text -> Mime.Mail
makeEmail uname email url =
	Mime.simpleMail'
		( Mime.Address (Just uname) email )
		( Mime.Address (Just $ DT.pack "JackRose Verification") (DT.pack "root@localhost") )
		( DT.pack "JackRose Account" )
		( DTL.concat [ DTL.pack "Your URL is ", DTL.fromChunks [ url ] ] )
