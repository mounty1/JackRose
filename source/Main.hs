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
import qualified Persistency (upgradeDB)
import qualified Configure (siteObject, portTCP)
import qualified CommandArgs (args)
import qualified JRState (JRState(..))
import qualified Application () -- just to get the instance and the dispatcher


-- | Start here.
main :: IO ()
-- ^ Turn the command line arguments into an easily-queried object,
-- use that to inform the construction of the foundation siteObject,
-- check that the authorised users table is in the current format,
-- and hand over to Warp, to launch the service.
main = CommandArgs.args >>= Configure.siteObject >>= letsGo


letsGo :: JRState.JRState -> IO ()
letsGo site = Authorisation.upgradeDB (JRState.tablesFile site)
	>> Persistency.upgradeDB (JRState.tablesFile site)
	>> Y.warp (Configure.portTCP site) site
