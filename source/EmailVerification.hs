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
import qualified TextItem (pack, TextItem, concat)
import qualified Network.Mail.SMTP as SMTP
import qualified Network.Mail.Mime as Mime
import qualified Logging
import qualified Data.Text.Lazy as DTL


newAccountEmail, resetAccountEmail :: (YC.MonadIO m, YC.MonadLogger m) => TextItem.TextItem -> TextItem.TextItem -> TextItem.TextItem -> m ()
newAccountEmail uname email url = do
	YC.liftIO $ SMTP.sendMail "localhost" (makeEmail uname email url)
	logAccountEmail uname email url "Verification"


resetAccountEmail uname email url = do
	YC.liftIO $ SMTP.sendMail "localhost" (makeEmail uname email url)
	logAccountEmail uname email url "Reset password"


logAccountEmail :: YC.MonadLogger m => TextItem.TextItem -> TextItem.TextItem -> TextItem.TextItem -> String -> m ()
logAccountEmail uname email url action =
	Logging.logInfo $ TextItem.concat [ TextItem.pack action, TextItem.pack " email for ", uname, TextItem.pack " (", email, TextItem.pack "): ", url ]


makeEmail :: TextItem.TextItem -> TextItem.TextItem -> TextItem.TextItem -> Mime.Mail
makeEmail uname email url =
	Mime.simpleMail'
		( Mime.Address (Just uname) email )
		( Mime.Address (Just $ TextItem.pack "JackRose Verification") (TextItem.pack "root@localhost") )
		( TextItem.pack "JackRose Account" )
		( DTL.concat [ DTL.pack "Your URL is ", DTL.fromChunks [ url ] ] )
