{-|
Description: Return an HTML failure message page
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}


module FailureMessage (page) where


import qualified Yesod.Core as YC
import qualified Data.Text as DT (Text)
import qualified Foundation (Handler)
import qualified JRState
import Control.Monad.Logger (logErrorNS)
import PresentHTML (toHTMLdoc, documentHTML)


-- | show failure as a web page
page :: DT.Text -> Foundation.Handler YC.Html
page msg = YC.getYesod >>= \site -> JRState.runFilteredLoggingT site (logErrorNS "internal" msg) >> toHTMLdoc (documentHTML msg)
