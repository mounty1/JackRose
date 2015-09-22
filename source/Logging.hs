{-|
Description: Log informational item
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

{-# LANGUAGE TemplateHaskell #-}

module Logging (logInfo, logWarn) where


import qualified Yesod.Core as YC
import qualified Data.Text as DT (Text)


logInfo :: YC.MonadLogger m => DT.Text -> m ()
logInfo message = $(YC.logInfo) message


logWarn :: YC.MonadLogger m => DT.Text -> m ()
logWarn message = $(YC.logWarn) message
