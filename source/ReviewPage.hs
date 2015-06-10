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
import qualified Data.Text as DT (Text, pack, unpack, concat, singleton)
import qualified Text.XML as XML
import qualified Text.Blaze.Html as BZH (toHtml)
import qualified Data.Map as DM
import qualified Data.List as DL (intersperse)
import qualified Data.Maybe as DMy


-- | show next item for review, for the logged-in user
review :: DT.Text -> Foundation.Handler YC.Html
review username = (YC.liftIO $ XML.readFile XML.def content) >>= return . BZH.toHtml . mashAround where
	fileName = DT.concat [username, DT.pack ".cfg"]
	content = DT.unpack fileName


mashAround (XML.Document _ content []) = XML.Document standardPrologue (embed $ divTop $ DMy.fromJust $ eXMLtract worm content) []


standardPrologue =
	XML.Prologue
		[XML.MiscInstruction (XML.Instruction (DT.pack "xml") (DT.pack "version=\"1.0\" encoding=\"UTF-8\""))]
		(Just (XML.Doctype (DT.pack "html") Nothing))
		[]


worm = map DT.pack [ "source", "category", "view", "back" ]


divTop :: XML.Element -> XML.Element
divTop (XML.Element _ attrs children) = XML.Element (nameXML "div") addClass children where
	addClass = DM.insertWith prepend (nameXML "class") (DT.pack "all") attrs
	prepend :: DT.Text -> DT.Text -> DT.Text
	prepend new old = DT.concat [new, DT.singleton ' ', old]


eXMLtract :: [DT.Text] -> XML.Element -> Maybe XML.Element
eXMLtract [] node = Just node
eXMLtract (name:rest) (XML.Element _ attrs children) =
	if null namedChildren then
		Nothing
	else
		Just $ head namedChildren where
	namedChildren = DMy.catMaybes $ map namedSubNode children
	namedSubNode (XML.NodeElement all@(XML.Element (XML.Name thisName _ _) _ _)) =
		if thisName == name then
			eXMLtract rest all
		else
			Nothing
	namedSubNode _ = Nothing


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
			[]
			[XML.NodeElement content,
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


oneSpace = XML.NodeContent $ DT.singleton ' '


nameXML string = XML.Name (DT.pack string) Nothing Nothing


postAttr = [makeAttribute "method" "post"]


alignAttr string = [makeAttribute "style" ("text-align:" ++ string ++ ";")]


button1 name = button name name


makeAttribute attr value = (nameXML attr, DT.pack value)


makeNode name attrs subs = XML.NodeElement $ XML.Element (nameXML name) (DM.fromList attrs) subs


button name value = makeNode "input" [makeAttribute "type" "submit",
					makeAttribute "name" name,
					makeAttribute "value" value] []


gradeButton digit = button "grade" [digit]


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
