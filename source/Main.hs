{-|
Module      : Main
Description : Entry point for JackRose
Copyright   : (c) Michael Mounteney, 2015
License     : BSD 3 clause
Maintainer  : jackrose@landcroft.com
Stability   : experimental
Portability : undefined

Jackrose is a @spaced repetition web service@.
-}

module Main where


import qualified Yesod as Y (warp)
import qualified Authorisation (upgradeDB)
import qualified Configure (siteObject, portTCP)
import qualified CommandArgs (args)
import qualified Foundation (JRState(..))
import qualified Application () -- just to get the instance and the dispatcher


-- | Start here
main :: IO ()
-- | Turn the command line arguments into a map of option letters to arguments,
-- | then use that to inform the construction of the foundation siteObject,
-- | then check that the authorisation table is in the current format,
-- | and finally hand over to Warp, to launch the service.
main = CommandArgs.args >>= Configure.siteObject >>= letsGo


letsGo :: Foundation.JRState -> IO ()
letsGo site = Authorisation.upgradeDB (Foundation.authTable site) >> Y.warp (Configure.portTCP site) site
