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
import qualified Authorisation (migrateData)
import qualified LearningData (migrateData)
import qualified DeckData (migrateData)
import qualified Persistency (upgradeDB)
import qualified CommandArgs (args)
import qualified JRState (JRState(..))
import qualified Application () -- just to get the instance and the dispatcher
import qualified LearningResync (update)
import Data.Maybe (fromMaybe)
import SiteConfigFromFile (siteObject)


-- | Start here.
main :: IO ()
-- | Turn the command line arguments into an easily-queried object,
-- use that to inform the construction of the foundation siteObject,
-- perform any required data migrations,
-- and hand over to Warp, to launch the service.
main = CommandArgs.args >>= siteObject >>= letsGo


letsGo :: JRState.JRState -> IO ()
letsGo site =
	mapM_ (Persistency.upgradeDB (JRState.tablesFile site)) [Authorisation.migrateData, LearningData.migrateData, DeckData.migrateData]
	>> LearningResync.update site
	>> Y.warp (fromMaybe (if JRState.secureOnly site then 443 else 80) (JRState.portNumber site)) site
