module EmailVerification(newAccountEmail, resetAccountEmail) where


import qualified Yesod.Core as YC
import qualified Pervasive (pack, TextItem, concat)
import qualified Network.Mail.SMTP as SMTP
import qualified Network.Mail.Mime as Mime
import qualified Logging
import qualified Data.Text.Lazy as DTL


newAccountEmail, resetAccountEmail :: (YC.MonadIO m, YC.MonadLogger m) => Pervasive.TextItem -> Pervasive.TextItem -> Pervasive.TextItem -> m ()
newAccountEmail uname email url = do
	YC.liftIO $ SMTP.sendMail "localhost" (makeEmail uname email url)
	logAccountEmail uname email url "Verification"


resetAccountEmail uname email url = do
	YC.liftIO $ SMTP.sendMail "localhost" (makeEmail uname email url)
	logAccountEmail uname email url "Reset password"


logAccountEmail :: YC.MonadLogger m => Pervasive.TextItem -> Pervasive.TextItem -> Pervasive.TextItem -> String -> m ()
logAccountEmail uname email url action =
	Logging.logInfo $ Pervasive.concat [ Pervasive.pack action, Pervasive.pack " email for ", uname, Pervasive.pack " (", email, Pervasive.pack "): ", url ]


makeEmail :: Pervasive.TextItem -> Pervasive.TextItem -> Pervasive.TextItem -> Mime.Mail
makeEmail uname email url =
	Mime.simpleMail'
		( Mime.Address (Just uname) email )
		( Mime.Address (Just $ Pervasive.pack "JackRose Verification") (Pervasive.pack "root@localhost") )
		( Pervasive.pack "JackRose Account" )
		( DTL.concat [ DTL.pack "Your URL is ", DTL.fromChunks [ url ] ] )
