{-|
Description: Filtering of logging from MonadLogger actions
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module LogFilter (runFilteredLoggingT) where


import Control.Monad.Logger (LoggingT, filterLogger, LogLevel, runStdoutLoggingT)
import Control.Monad.IO.Class (MonadIO)


-- | Logging to standard output.
runFilteredLoggingT :: MonadIO m => LogLevel -> LoggingT m a -> m a
runFilteredLoggingT level = runStdoutLoggingT . filterLogger (\_ -> (<=) level)
