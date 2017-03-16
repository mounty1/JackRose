{-|
Description: give the application icon
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

It could be argued that we should use the Branding name here but favicon is 'well-known'
and expected in this context.
-}


{-# LANGUAGE OverloadedStrings #-}


module Favicon (getIconR) where


import Foundation (Handler)
import Yesod.Core.Handler (sendFile)


-- | return application icon.
getIconR :: Handler ()
getIconR = sendFile "image/x-icon" "favicon.ico"
