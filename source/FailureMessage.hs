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


-- | show next item for review, for the logged-in user
page :: DT.Text -> Foundation.Handler YC.Html
page msg = YC.defaultLayout [YC.whamlet|<H1>#{msg}|]
