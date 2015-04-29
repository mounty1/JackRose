{-|
Module: Main
Description: It all starts here.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Jackrose is a spaced repetition web service.
-}

module Main (main) where


import qualified Yesod as Y (warp)
import qualified Authorisation (upgradeDB)
import qualified Configure (siteObject, portTCP)
import qualified CommandArgs (args)
import qualified Foundation (JRState(..))
import qualified Application () -- just to get the instance and the dispatcher


-- | Start here.
main :: IO ()
-- ^ Turn the command line arguments into an easily-queried object,
-- use that to inform the construction of the foundation siteObject,
-- check that the authorised users table is in the current format,
-- and hand over to Warp, to launch the service.
main = CommandArgs.args >>= Configure.siteObject >>= letsGo


letsGo :: Foundation.JRState -> IO ()
letsGo site = Authorisation.upgradeDB (Foundation.authTable site) >> Y.warp (Configure.portTCP site) site
