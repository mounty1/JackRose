{-|
Description: Return content of next review item
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}


module ReviewGet (getHomeR, getReviewR) where


import qualified Yesod.Core as YC
import Yesod.Core.Handler (setSession)
import qualified Foundation (Handler)
import qualified Data.Text as DT (Text, split, singleton, concat, null)
import qualified Text.XML as XML
import qualified Text.Blaze.Html as BZH (toHtml)
import qualified Data.Map as DM
import qualified Data.List as DL (intersperse, filter)
import LoginPlease (onlyIfAuthorised)
import qualified JRState (runFilteredLoggingT, getUserConfig, JRState, tablesFile)
import qualified UserDeck (UserDeckCpt(..), NewThrottle)
import LearningData (ViewId, LearnDatumId, LearnDatum(..), newItem, dueItem, getLearnDatumKey)
import Data.Time (getCurrentTime)
import Authorisation (UserId)
import Database.Persist.Sqlite (runSqlPool)
import TextShow (showt)
import Data.Maybe (listToMaybe)
import Database.Persist.Sql (Entity(Entity), fromSqlKey)
import Database.Persist.Sql (SqlPersistT)
import Control.Monad.Logger (LoggingT)


type PresentationParams = (LearnDatumId, XML.Document)


-- | verify that a user be logged-in, and if s/he be, present the next item for review.
getHomeR :: Foundation.Handler YC.Html
getHomeR = onlyIfAuthorised (review [])


-- | show next item for review, for the logged-in user
getReviewR :: DT.Text -> Foundation.Handler YC.Html
getReviewR deckSteck = onlyIfAuthorised (review $ splitSlash deckSteck)


review :: [DT.Text] -> DT.Text -> Foundation.Handler YC.Html
review deckPath {- | e.g., ["Language", "Alphabets", "Arabic"] -} username = YC.getYesod >>= zappo where
		-- TODO: for no-user case, just go back to the login screen, but with the message;  then lose informationMessage
			zappo site = (YC.liftIO $ JRState.getUserConfig site) >>=
				maybe (informationMessage $ DT.concat ["user \"", username, "\" dropped from state"]) (descendToDeckRoot site Nothing deckPath) . DM.lookup username


splitSlash :: DT.Text -> [DT.Text]
splitSlash = DL.filter (not . DT.null) . DT.split (== '/')


descendToDeckRoot :: JRState.JRState -> UserDeck.NewThrottle -> [DT.Text] -> (UserId, [UserDeck.UserDeckCpt]) -> Foundation.Handler YC.Html

-- Still nodes to descend in requested sub-tree, but nowhere to go
descendToDeckRoot _ _ _ (_, []) = informationMessage "nowhere to go"

-- If XPath-like spec. is empty then 'stuff' is the requested sub-tree so pick an item to display.
-- This is significant inasmuch as if it be deterministic, there will be no randomness if the user goes away then tries again later.
-- The algorithm must present an item if any be due;  otherwise a 'new' item, constrained by the cascaded throttle,
-- which is the daily (well, sliding 24 hour window) limit on new cards.
descendToDeckRoot site throttle [] (userId, stuff : _) = searchForExistingByTable site userId flattenedDeck >>= maybe fallToNew rememberItem where
		fallToNew = searchForNew site userId throttle flattenedDeck >>= maybe (informationMessage "no more items") rememberItem
		flattenedDeck = tableList stuff

-- both XPath-like and tree;  find a matching node (if it exist) and descend
descendToDeckRoot site throttle deckPath@(d1 : dn) (userId, UserDeck.SubDeck maybeThrottle shuffle label item : rest) =
	if d1 == label then descendToDeckRoot site newThrottle dn (userId, item) else descendToDeckRoot site throttle deckPath (userId, rest) where
		newThrottle = mergeThrottle throttle maybeThrottle

-- no match so try next peer
descendToDeckRoot site throttle deckPath (userId, _ : rest) = descendToDeckRoot site throttle deckPath (userId, rest)


-- TODO document
searchForExistingByTable :: JRState.JRState -> UserId -> [ViewId] -> Foundation.Handler (Maybe PresentationParams)

-- descended to a terminal node (a TableView) so get the next card from it
searchForExistingByTable site userId views =YC.liftIO $ getCurrentTime >>= showCardItem where
	showCardItem now = runItemQuery site (dueItem userId now views)


tableList :: UserDeck.UserDeckCpt -> [ViewId]
tableList (UserDeck.TableView _ _ vid _) = [vid]
tableList (UserDeck.SubDeck _ _ _ dex) = concatMap tableList dex


getItemAndViewIds :: Entity LearnDatum -> PresentationParams
getItemAndViewIds (Entity i _) = (i, XML.Document standardPrologue (embed [XML.NodeContent "obverse side"]) [])


searchForNew ::JRState.JRState -> UserId -> UserDeck.NewThrottle -> [ViewId] -> Foundation.Handler (Maybe PresentationParams)
searchForNew site userId throttle views = runItemQuery site (newItem userId views)


runItemQuery :: (YC.MonadIO m, YC.MonadBaseControl IO m) => JRState.JRState -> SqlPersistT (LoggingT m) [Entity LearnDatum] -> m (Maybe PresentationParams)
runItemQuery site fn = JRState.runFilteredLoggingT site (runSqlPool fn (JRState.tablesFile site)) >>= return . fmap getItemAndViewIds . listToMaybe


-- put the necessary data into the session so that the POST knows what to add or update.
rememberItem :: PresentationParams -> Foundation.Handler YC.Html
rememberItem (item, document) = setSession "JR.view" (showt $ fromSqlKey viewId)
		>> setSession "JR.row" (showt $ fromSqlKey rowId)
		>> YC.liftIO (BZH.toHtml `fmap` return document) where
		(viewId, rowId, _) = getLearnDatumKey item


mergeThrottle :: UserDeck.NewThrottle -> UserDeck.NewThrottle -> UserDeck.NewThrottle
mergeThrottle Nothing newThrottle = newThrottle
mergeThrottle already Nothing = already
mergeThrottle a@(Just already) n@(Just newThrottle) = if already < newThrottle then a else n


informationMessage :: DT.Text -> Foundation.Handler YC.Html
informationMessage message = YC.liftIO $ fmap BZH.toHtml $ documentHTML [XML.NodeContent message]


documentHTML :: [XML.Node] -> IO XML.Document
documentHTML content = return $ XML.Document standardPrologue (embed content) []


standardPrologue :: XML.Prologue
standardPrologue =
	XML.Prologue
		[XML.MiscInstruction (XML.Instruction "xml" "version=\"1.0\" encoding=\"UTF-8\"")]
		(Just (XML.Doctype "html" Nothing))
		[]


embed :: [XML.Node] -> XML.Element
embed content =
	XML.Element
		(nameXML "html")
		DM.empty
		[makeNode "head"
			[]
			[makeNode "meta"
				[makeAttribute "http-equiv" "Content-Type",
					makeAttribute "content" "application/xhtml+xml;charset=utf-8"]
				[],
			makeNode "title" [] [XML.NodeContent "Greek / Grammar-"],
			makeNode "style" [] [XML.NodeContent ceeSS]
			],
		makeNode "body"
			[makeAttribute "class" "all"]
			[makeNode "div" [] content,
			makeNode "div" [] [makeNode "hr" [] []],
			makeNode "table"
				[makeAttribute "width" "100%"]
				[makeNode "tr"
					[]
					[makeNode "td"
						(alignAttr "left")
						[makeNode "form" postAttr [button1 "stats"]],
					makeNode "td"
						(alignAttr "center")
						[makeNode "form"
							postAttr
							(DL.intersperse oneSpace $ map gradeButton ['0' .. '9'])],
					makeNode "td"
						(alignAttr "right")
						[makeNode "form" postAttr [button1 "logout"]]
				]
			]
		]
	]


oneSpace :: XML.Node
oneSpace = XML.NodeContent $ DT.singleton ' '


nameXML :: DT.Text -> XML.Name
nameXML tag = XML.Name tag Nothing Nothing


postAttr :: [(XML.Name, DT.Text)]
postAttr = [makeAttribute "method" "post"]


alignAttr :: DT.Text -> [(XML.Name, DT.Text)]
alignAttr alignment = [makeAttribute "style" (DT.concat ["text-align:", alignment, ";"])]


button1 :: DT.Text -> XML.Node
button1 name = button name name


makeAttribute :: DT.Text -> DT.Text -> (XML.Name, DT.Text)
makeAttribute attr value = (nameXML attr, value)


makeNode :: DT.Text -> [(XML.Name, DT.Text)] -> [XML.Node] -> XML.Node
makeNode name attrs subs = XML.NodeElement $ XML.Element (nameXML name) (DM.fromList attrs) subs


button :: DT.Text -> DT.Text -> XML.Node
button name value = makeNode "input" [makeAttribute "type" "submit",
					makeAttribute "name" name,
					makeAttribute "value" value] []


gradeButton :: Char -> XML.Node
gradeButton digit = button "grade" (DT.singleton digit)


ceeSS :: DT.Text
ceeSS = ".all {\n\
     \font-family: Code2000;\n\
     \font-size: 24pt;\n\
     \background-color: #00ff80;\n\
     \text-align: center;\n\
\}\n\
\td {\n\
     \text-align: left;\n\
\}\n\
\.greek {\n\
     \color: #20198c;\n\
\}\n\
\*[mood=I]::before { content: \"Ⓘ\"; }\n\
\*[mood=i]::before { content: \"ⓘ\"; }\n\
\*[mood=O]::before { content: \"Ⓞ\"; }\n\
\*[mood=S]::before { content: \"Ⓢ\"; }\n\
\*[tense=r]::after { content: \"præs.\"; }\n\
\*[tense=f]::after { content: \"fut.\"; }\n\
\*[tense=m]::after { content: \"impf.\"; }\n\
\*[tense=R]::after { content: \"Ⓡ\"; }\n\
\*[tense=k]::after { content: \"perf.\"; }\n\
\*[tense=Q]::after { content: \"Ⓠ\"; }\n\
\*[voice=A]::after { content: \"Ⓐ\"; }\n\
\*[voice=M]::after { content: \"Ⓜ\"; }\n\
\*[voice=MP]::after { content: \"ⓂⓅ\"; }\n\
\*[voice=D]::after { content: \"Ⓟ\"; }\n\
\.comment {\n\
     \font-size: 16pt;\n\
     \font-style: italic;\n\
     \text-align: left;\n\
\}\n\
\.instruction {\n\
     \text-align: right;\n\
     \font-size: 20pt;\n\
     \font-style: italic;\n\
\}\n\
\* {\n\
	\margin: 0;\n\
\}\n\
\html, body {\n\
	\height: 100%;\n\
\}\n\
\div:first-child {\n\
	\min-height: 100%;\n\
	\height: auto !important;\n\
	\height: 100%;\n\
	\margin: 0 auto -3em;\n\
\}\n\
\div + div {\n\
	\height: 1em;\n\
\}\n\
\html body table tbody tr td form {\n\
     \font-size: 20pt;\n\
	\text-align: center;\n\
\}"
