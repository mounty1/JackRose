{-|
Description: assemble the tree 'deck' structure for a user.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Take the users's configuration file and pick out the data sources and other information.
If an unrecoverable error occurs, return a text diagnostic in the Left constructor;
otherwise, return an object of the schema data.  Since the code is within the
@LoggingT IO@ monad, there are full diagnostics etc. output.  As soon as an error be
encountered, the definition of our monad means that evaluation backs out immediately.
-}


{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}


module DeckSpec (content) where


import qualified Yesod as Y
import qualified Data.Text as DT (Text)
import qualified JRState
import Data.Maybe (isNothing, fromMaybe)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Control (liftBaseWith, restoreM, StM)
import qualified UserDeck (UserDeckCpt(..))
import qualified DeckData as DD
import Database.Persist.Types (entityVal)
import Authorisation (UserId)
import Database.Persist.Sql as DPQ (runSqlPool, Entity(Entity))


newtype ParsingResult a = ParsingResult (LoggingT IO (Either DT.Text a))


unwrapPR :: ParsingResult t -> LoggingT IO (Either DT.Text t)

unwrapPR (ParsingResult value) = value


instance Applicative ParsingResult where
	pure = ParsingResult . return . Right
	ParsingResult a <*> ParsingResult b = ParsingResult $ a >>= (\f -> b >>= return . (<*>) f)


instance Monad ParsingResult where
	return = ParsingResult . return . Right
	ParsingResult v >>= f = ParsingResult $ v >>= either (return . Left) (unwrapPR . f)


instance Functor ParsingResult where
	fmap f (ParsingResult v) = ParsingResult $ fmap (fmap f) v


instance Y.MonadIO ParsingResult where
	liftIO = ParsingResult . Y.lift . fmap Right


instance Y.MonadBase IO ParsingResult where
	liftBase = ParsingResult . Y.liftBase . fmap Right


instance Y.MonadBaseControl IO ParsingResult where
	type StM ParsingResult a = Either DT.Text a
	liftBaseWith f = ParsingResult $ liftBaseWith $ \q -> Right `fmap` f (q . unwrapPR)
	restoreM = ParsingResult . restoreM


content :: UserId -> JRState.JRState -> IO (Either DT.Text [UserDeck.UserDeckCpt])
-- | Parse the users's configuration file.  This might fail (returning a @Left DT.Text@)
-- or succeed (returning a @Right [UserDeck.UserDeckCpt]).
-- Currently, it does not in fact ever fail.  If its implementation changes, retrieve
-- function failToParse from VC (2017.Feb.3).
content userId site = JRState.runFilteredLoggingT site $ unwrapPR $ topDeck userId site


topDeck :: UserId -> JRState.JRState -> ParsingResult [UserDeck.UserDeckCpt]
topDeck uid site = DPQ.runSqlPool (DD.userDeckEnds uid) (JRState.tablesFile site) >>=
		\terminals -> DPQ.runSqlPool (DD.userDeckNodes uid) (JRState.tablesFile site) >>=
			return . levelFilter isNothing (JRState.shuffleCards site) terminals


levelFilter :: (Maybe DD.UserDeckNodeId -> Bool) -> Bool -> [DPQ.Entity DD.UserDeckEnd] -> [DPQ.Entity DD.UserDeckNode] -> [UserDeck.UserDeckCpt]
levelFilter criterion shuffleDeck terminals nodes =
		map (fromTerm shuffleDeck) (filter (criterion . DD.userDeckEndParent . entityVal) terminals) ++
		map (fromNode shuffleDeck terminals nodes) (filter (criterion . DD.userDeckNodeParent . entityVal) nodes)


fromTerm :: Bool -> DPQ.Entity DD.UserDeckEnd -> UserDeck.UserDeckCpt
fromTerm shuffleDeck (DPQ.Entity _ terminal) = UserDeck.TableView
		(DD.userDeckEndThrottle terminal)
		(fromMaybe shuffleDeck $ DD.userDeckEndShuffle terminal)
		(DD.userDeckEndViewId terminal)


-- a nodes children are all the nodes whose parent == this nodes id.
fromNode :: Bool -> [DPQ.Entity DD.UserDeckEnd] -> [DPQ.Entity DD.UserDeckNode] -> DPQ.Entity DD.UserDeckNode -> UserDeck.UserDeckCpt
fromNode shuffleDeck terminals nodes (DPQ.Entity nodeId node) = UserDeck.SubDeck
		(DD.userDeckNodeThrottle node)
		shufflety
		(DD.userDeckNodeLabel node)
		(levelFilter (maybe False ((==) nodeId)) shufflety terminals nodes) where
			shufflety = fromMaybe shuffleDeck $ DD.userDeckNodeShuffle node
