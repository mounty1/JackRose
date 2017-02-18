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


module DeckSpec (SchemaParsing, content) where


import qualified Yesod as Y
import qualified Data.Text as DT (Text)
import qualified Data.Map as DM
import qualified JRState
import Data.Maybe (isNothing, fromMaybe)
import Control.Monad.Logger (LoggingT, logErrorNS)
import Control.Monad.Trans.Control (liftBaseWith, restoreM, StM)
import MaybeIntValue (maybeIntValue)
import qualified UserDeck (UserDeckCpt(..))
import qualified DeckData as DD
import Database.Persist.Types (entityVal)
import qualified ViewSpec as VS (View(..))
import Authorisation (UserId)
import Database.Persist.Sqlite (runSqlPool)
import Database.Persist.Sql as DPQ (Entity(Entity))


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

type SchemaParsing = ParsingResult [UserDeck.UserDeckCpt]


-- | Parse the users's configuration file.  This might fail (returning a @Left DT.Text@)
-- | or succeed (returning a @Right [UserDeck.UserDeckCpt]).
content :: UserId -> JRState.JRState -> IO (Either DT.Text [UserDeck.UserDeckCpt])

content uid site = (JRState.getViews site >>= JRState.runFilteredLoggingT site . unwrapPR . topDeck site uid)


topDeck :: JRState.JRState -> UserId -> JRState.DataViews -> ParsingResult [UserDeck.UserDeckCpt]
topDeck site uid deckViews = runSqlPool (DD.userDeckEnds uid) (JRState.tablesFile site) >>=
		\terminals -> runSqlPool (DD.userDeckNodes uid) (JRState.tablesFile site) >>=
			return . levelFilter isNothing deckViews (JRState.shuffleCards site) terminals


levelFilter :: (Maybe DD.UserDeckNodeId -> Bool) -> JRState.DataViews -> Bool -> [DPQ.Entity DD.UserDeckEnd] -> [DPQ.Entity DD.UserDeckNode] -> [UserDeck.UserDeckCpt]
levelFilter criterion deckViews shuffleDeck terminals nodes =
		map (fromTerm deckViews shuffleDeck) (filter (criterion . DD.userDeckEndParent . entityVal) terminals) ++
		map (fromNode deckViews shuffleDeck terminals nodes) (filter (criterion . DD.userDeckNodeParent . entityVal) nodes)


fromTerm :: JRState.DataViews -> Bool -> DPQ.Entity DD.UserDeckEnd -> UserDeck.UserDeckCpt
fromTerm deckViews shuffleDeck (DPQ.Entity _ terminal) = UserDeck.TableView
		(DD.userDeckEndThrottle terminal)
		(fromMaybe shuffleDeck $ DD.userDeckEndShuffle terminal)
		(DD.userDeckEndViewId terminal)
		(VS.openConnection $ deckViews DM.! (DD.userDeckEndViewId terminal))


-- a nodes children are all the nodes whose parent == this nodes id.
fromNode :: JRState.DataViews -> Bool -> [DPQ.Entity DD.UserDeckEnd] -> [DPQ.Entity DD.UserDeckNode] -> DPQ.Entity DD.UserDeckNode -> UserDeck.UserDeckCpt
fromNode deckViews shuffleDeck terminals nodes (DPQ.Entity nodeId node) = UserDeck.SubDeck
		(DD.userDeckNodeThrottle node)
		shufflety
		(DD.userDeckNodeLabel node)
		(levelFilter (maybe False ((==) nodeId)) deckViews shufflety terminals nodes) where
			shufflety = fromMaybe shuffleDeck $ DD.userDeckNodeShuffle node


logSource :: DT.Text
logSource = "user-schema"


-- Fail to parse.  This is a quite crucial function because it returns a Left value;
-- thus, all the @<$>@ and @>>=@ will just pass through the failure message and eventually
-- return it to the caller.
failToParse :: DT.Text -> ParsingResult a
failToParse message = ParsingResult $ logErrorNS logSource message >> (return $ Left message)
