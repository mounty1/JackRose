{-|
Description: Programming common to {Review,Score}Get screens.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module CardItemGet (PresentationParams, rememberItem) where


import qualified Yesod.Core as YC
import qualified Foundation
import qualified FailureMessage (page)
import qualified Data.Text as DT (Text)
import qualified Text.XML as XML (Document)
import qualified PresentHTML as PH
import qualified SessionItemKey (set)
import Database.Persist.Sql (Key)
import LearningData (LearnDatum(..))


-- | Parameters of item about to be displayed, or an explanation of why it's not available.
type PresentationParams = Either DT.Text (Key LearnDatum, XML.Document)


-- | Put the necessary data into the session so that the POST knows what to add or update.
-- TODO a Left indicates inconsistency in the data, so really we should log every one out and fix it.
rememberItem :: PresentationParams -> Foundation.Handler YC.Html
rememberItem (Left errCode) = FailureMessage.page errCode
rememberItem (Right (itemId, document)) = SessionItemKey.set itemId >> PH.toHTMLdoc document
