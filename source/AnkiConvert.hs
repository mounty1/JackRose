{-|
Module: Copy from an Anki database into JackRose.
Description: It all starts here.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Because we re-use the start-up configuration framework for JR itself, there's no room for the
Anki database name so it is hard-wired to "anki.sqlite".
-}

{-# LANGUAGE OverloadedStrings #-}


module Main (main) where


import qualified Authorisation (migrateData)
import qualified LearningData (migrateData)
import qualified DeckData (migrateData)
import qualified Persistency (upgradeDB)
import qualified CommandArgs (args)
import qualified JRState (JRState(..))
import qualified LearningResync (update)
import SiteConfigFromFile (siteObject)
import qualified Database.Persist.Sqlite as PerstQ (createSqlitePool, ConnectionPool)
import AnkiSchema ()
import LogFilter (runFilteredLoggingT)
import Control.Monad.Logger (LogLevel(..))


-- | Start here.
main :: IO ()
-- | Turn the command line arguments into an easily-queried object,
-- use that to inform the construction of the foundation siteObject,
-- then invoke the conversion.
main = CommandArgs.args >>= siteObject >>= letsGo


letsGo :: JRState.JRState -> IO ()
letsGo site =
	mapM_ (Persistency.upgradeDB (JRState.tablesFile site)) [Authorisation.migrateData, LearningData.migrateData, DeckData.migrateData]
	>> LearningResync.update site
	>> runFilteredLoggingT LevelInfo (PerstQ.createSqlitePool "anki.sqlite" 1)
	>>= copyConvertAnki site


copyConvertAnki :: JRState.JRState -> PerstQ.ConnectionPool -> IO ()
copyConvertAnki site sourceAnki = putStrLn "incomplete"
