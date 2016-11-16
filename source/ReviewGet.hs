{-|
Description: Return content of next review item
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module ReviewGet (getHomeR, getReviewR) where


import qualified Yesod.Core as YC
import qualified Foundation (Handler)
import qualified Data.Text as DT (Text, split, singleton, concat, null)
import qualified Text.XML as XML
import qualified Text.Blaze.Html as BZH (toHtml)
import qualified Data.Map as DM
import qualified Data.List as DL (intersperse, filter)
import LoginPlease (onlyIfAuthorised)
import qualified JRState (getUserConfig)
import qualified UserDeck (UserDeckCpt(..), NewThrottle)
import LearningData (LearnDatum(..), dueItems)
import Data.Time (getCurrentTime)


-- | verify that a user be logged-in, and if s/he be, present the next item for review.
getHomeR :: Foundation.Handler YC.Html
getHomeR = onlyIfAuthorised (review [])


-- | show next item for review, for the logged-in user
getReviewR :: DT.Text -> Foundation.Handler YC.Html
getReviewR deckSteck = onlyIfAuthorised (review $ splitSlash deckSteck)


review :: [DT.Text] -> DT.Text -> Foundation.Handler YC.Html
review deckPath {- | e.g., ["Language", "Alphabets", "Arabic"] -} username = YC.getYesod
		>>= YC.liftIO . JRState.getUserConfig
		>>= YC.liftIO . fmap BZH.toHtml . maybe (errorNoUser username) (drillDeckStack Nothing deckPath) . DM.lookup username


splitSlash :: DT.Text -> [DT.Text]
splitSlash = DL.filter (not . DT.null) . DT.split (== '/')


errorNoUser :: DT.Text -> IO XML.Document
errorNoUser username = informationMessage $ DT.concat ["user \"", username, "\" dropped from state"]


{- ALGORITHM:
	NewThrottle = Nothing
	Randomise = setting from config.
	While deckPath != []:
		NewThrottle = NewThrottle >>= fn(limit at this level, new cards in 24 hours)
		Randomise = Randomise >> (setting from level)
	If it's a View
		if cards available:
			show a card (random)
		elif NewThrottle > 0
			show a new card
		else
			no more
	If it's a (sub-)deck:
		If Random:
			Depth-first search for View with cards available
			if cards available
				show a card (random)

			If some:
				Any due items.
				New items to all cascaded throttle.
			else:
				depth-first search for first unshuffled with items.
				Any due items.
				New items to all cascaded throttle.
		else:
			depth-first search for first Deck/View with items.
PROBLEM:  calculation of used throttle.
 -}
drillDeckStack :: UserDeck.NewThrottle -> [DT.Text] -> [UserDeck.UserDeckCpt] -> IO XML.Document

-- Still nodes to descend in requested sub-tree, but nowhere to go
drillDeckStack _ _ [] = informationMessage "no such deck"

-- XPath-like spec. is empty;  'stuff' is the requested sub-tree so pick an item to display.
-- This is significant inasmuch as if it be deterministic, there will be no randomness if the user goes away then tries again later.
-- The algorithm must present an item if any be due;  otherwise a 'new' item, constrained by the cascaded daily throttling.
drillDeckStack throttle [] stuff = drillDownDeck throttle stuff

-- both XPath-like and tree;  find a matching node (if it exist) and descend
drillDeckStack throttle deckPath@(d1 : dn) (UserDeck.SubDeck maybeThrottle shuffle label item : rest) =
	if d1 == label then drillDeckStack (mergeThrottle throttle maybeThrottle) dn item else drillDeckStack throttle deckPath rest

-- no match so try next peer
drillDeckStack throttle deckPath (_ : rest) =
	drillDeckStack throttle deckPath rest


drillDownDeck :: UserDeck.NewThrottle -> [UserDeck.UserDeckCpt] -> IO XML.Document

-- descended to a terminal node (a TableView) so get the next card from it
drillDownDeck throttle (UserDeck.TableView _ _ _ dataSource obverse _ : _) = getCurrentTime >>= showCardItem where
	showCardItem now = return $ XML.Document standardPrologue (embed obverse) []


mergeThrottle :: UserDeck.NewThrottle -> UserDeck.NewThrottle -> UserDeck.NewThrottle
mergeThrottle Nothing newThrottle = newThrottle
mergeThrottle already Nothing = already
mergeThrottle (Just already) (Just newThrottle) = Just (if already < newThrottle then already else newThrottle)


informationMessage :: DT.Text -> IO XML.Document
informationMessage message = documentHTML [XML.NodeContent message]


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
