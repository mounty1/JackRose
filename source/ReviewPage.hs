{-|
Description: Return content of next review item
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module ReviewPage (review) where


import qualified Yesod.Core as YC
import qualified Foundation
import qualified Data.Text as DT (Text, pack, unpack, singleton, append)
import qualified Text.XML as XML
import qualified Text.Blaze.Html as BZH (toHtml)
import qualified Data.Map as DM
import qualified Data.List as DL (intersperse)
import qualified ConfigParse (content, UserSchema(..), Logged(..), SchemaParsing, View(..))
import qualified FailureMessage (page)
import qualified Logging
import qualified Data.Maybe as DMy (fromMaybe)


-- | show next item for review, for the logged-in user
review :: DT.Text -> Foundation.Handler YC.Html
review username = YC.getYesod >>= pong where
	pong base = (YC.liftIO $ XML.readFile XML.def (DT.unpack $ Foundation.userDir base `DT.append` contentName)) >>= digest . ConfigParse.content contentName (Foundation.debugging base) where
	contentName = username `DT.append` (DT.pack ".cfg")


digest :: ConfigParse.SchemaParsing -> Foundation.Handler YC.Html

digest (Left failReason) = FailureMessage.page failReason

digest (Right (ConfigParse.Logged warnings info (ConfigParse.UserSchema _ views))) =
	mapM_ Logging.logWarn warnings >> mapM_ Logging.logInfo (DMy.fromMaybe [] info) >> (return $ BZH.toHtml $ mashAround views)


mashAround :: [ConfigParse.View] -> XML.Document
mashAround (ConfigParse.View _ _ obverse _ : _) = XML.Document standardPrologue (embed obverse) []
mashAround [] = XML.Document standardPrologue (embed []) []


standardPrologue :: XML.Prologue
standardPrologue =
	XML.Prologue
		[XML.MiscInstruction (XML.Instruction (DT.pack "xml") (DT.pack "version=\"1.0\" encoding=\"UTF-8\""))]
		(Just (XML.Doctype (DT.pack "html") Nothing))
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
			makeNode "title" [] [XML.NodeContent $ DT.pack "Greek / Grammar-"],
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
						[makeNode "form" postAttr [button1 "load"]],
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


nameXML :: String -> XML.Name
nameXML string = XML.Name (DT.pack string) Nothing Nothing


postAttr :: [(XML.Name, DT.Text)]
postAttr = [makeAttribute "method" "post"]


alignAttr :: [Char] -> [(XML.Name, DT.Text)]
alignAttr string = [makeAttribute "style" ("text-align:" ++ string ++ ";")]


button1 :: String -> XML.Node
button1 name = button name name


makeAttribute :: String -> String -> (XML.Name, DT.Text)
makeAttribute attr value = (nameXML attr, DT.pack value)


makeNode :: String -> [(XML.Name, DT.Text)] -> [XML.Node] -> XML.Node
makeNode name attrs subs = XML.NodeElement $ XML.Element (nameXML name) (DM.fromList attrs) subs


button :: String -> String -> XML.Node
button name value = makeNode "input" [makeAttribute "type" "submit",
					makeAttribute "name" name,
					makeAttribute "value" value] []


gradeButton :: Char -> XML.Node
gradeButton digit = button "grade" [digit]


ceeSS :: DT.Text
ceeSS = DT.pack ".all {\n\
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
