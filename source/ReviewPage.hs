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
import qualified Data.Text as DT (Text, pack, concat, singleton)
import qualified Text.XML as XML
import qualified Filesystem.Path.CurrentOS as FP
import qualified Text.Blaze.Html as BZH (toHtml)
import qualified Data.Map as DM
import qualified Data.List as DL (intersperse)


-- | show next item for review, for the logged-in user
review :: DT.Text -> Foundation.Handler YC.Html
review username = (YC.liftIO $ XML.readFile XML.def content) >>= return . BZH.toHtml . mashAround where
	fileName = DT.concat [username, DT.pack ".xml"]
	content = FP.fromText fileName


mashAround (XML.Document (XML.Prologue [] Nothing []) content []) = XML.Document standardPrologue (embed content) []


standardPrologue =
	XML.Prologue
		[XML.MiscInstruction (XML.Instruction (DT.pack "xml") (DT.pack "version=\"1.0\" encoding=\"UTF-8\""))]
		(Just (XML.Doctype (DT.pack "html") Nothing))
		[]


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


ceeSS = DT.pack ".all {\
     \font-family: Code2000;\
     \font-size: 24pt;\
     \background-color: #00ff80;\
     \text-align: center;\
\}\
\td {\
     \text-align: left;\
\}\
\.greek {\
     \color: #20198c;\
\}\
\*[mood=I]::before { content: \"Ⓘ\"; }\
\*[mood=i]::before { content: \"ⓘ\"; }\
\*[mood=O]::before { content: \"Ⓞ\"; }\
\*[mood=S]::before { content: \"Ⓢ\"; }\
\*[tense=r]::after { content: \"præs.\"; }\
\*[tense=f]::after { content: \"fut.\"; }\
\*[tense=m]::after { content: \"impf.\"; }\
\*[tense=R]::after { content: \"Ⓡ\"; }\
\*[tense=k]::after { content: \"perf.\"; }\
\*[tense=Q]::after { content: \"Ⓠ\"; }\
\*[voice=A]::after { content: \"Ⓐ\"; }\
\*[voice=M]::after { content: \"Ⓜ\"; }\
\*[voice=MP]::after { content: \"ⓂⓅ\"; }\
\*[voice=D]::after { content: \"Ⓟ\"; }\
\.comment {\
     \font-size: 16pt;\
     \font-style: italic;\
     \text-align: left;\
\}\
\.instruction {\
     \text-align: right;\
     \font-size: 20pt;\
     \font-style: italic;\
\}\
\* {\
	\margin: 0;\
\}\
\html, body {\
	\height: 100%;\
\}\
\div:first-child {\
	\min-height: 100%;\
	\height: auto !important;\
	\height: 100%;\
	\margin: 0 auto -3em;\
\}\
\div + div {\
	\height: 1em;\
\}\
\html body table tr td form {\
     \font-size: 20pt;\
	\text-align: center;\
\}"
