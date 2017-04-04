{-|
Description: Return an HTML failure message page
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module FailureMessage (page) where


import qualified Yesod.Core as YC
import qualified Data.Text as DT (Text)
import qualified Foundation
import qualified JRState
import Control.Monad.Logger (logErrorNS)


-- | Show failure as a web page.
-- Just log the error then redirect to the Notice page.
-- This isn't a route because we don't want unauthorised users to log messages.
page :: DT.Text -> Foundation.Handler YC.Html
page msg = YC.getYesod >>= \site -> JRState.runFilteredLoggingT site (logErrorNS "internal" msg) >> YC.redirect (Foundation.NoticeR msg)
