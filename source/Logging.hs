{-|
Description: Log informational item
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

{-# LANGUAGE TemplateHaskell #-}

module Logging (logInfo) where


import qualified Yesod.Core as YC
import qualified TextItem (TextItem)


logInfo :: YC.MonadLogger m => TextItem.TextItem -> m ()
logInfo message = $(YC.logInfo) message
