{-# LANGUAGE TemplateHaskell #-}

module Logging(logInfo) where


import qualified Yesod.Core as YC
import qualified Pervasive (TextItem)


logInfo :: YC.MonadLogger m => Pervasive.TextItem -> m ()
logInfo message = $(YC.logInfo) message
