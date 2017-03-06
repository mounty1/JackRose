{-|
Description: Dump-bag of HTML formatting and stuff.  Must be rationalised.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module PresentHTML where


import qualified Yesod.Core as YC
import qualified Foundation (Handler)
import qualified Data.Text as DT (Text, singleton, concat)
import qualified Text.XML as XML
import qualified Text.Blaze.Html as BZH (toHtml)
import qualified Data.Map as DM
import qualified Data.List as DL (intersperse)


toHTMLdoc :: XML.Document -> Foundation.Handler YC.Html
toHTMLdoc = YC.liftIO . return . BZH.toHtml


documentHTML :: DT.Text -> XML.Document
documentHTML content = XML.Document standardPrologue (embed [XML.NodeContent content] okButton) []


documentXHTML :: [XML.Node] ->  [XML.Node] -> XML.Document
documentXHTML buttons content = XML.Document standardPrologue (embed content buttons) []


standardPrologue :: XML.Prologue
standardPrologue =
	XML.Prologue
		[XML.MiscInstruction (XML.Instruction "xml" "version=\"1.0\" encoding=\"UTF-8\"")]
		(Just (XML.Doctype "html" Nothing))
		[]


embed :: [XML.Node] -> [XML.Node] -> XML.Element
embed content nextButton =
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
						[makeNode "form" postAttr nextButton],
					makeNode "td"
						(alignAttr "right")
						[makeNode "form" postAttr [button1 "logout"]]
				]
			]
		]
	]


okButton, gradeButtons :: [XML.Node]


-- label must be synchronised with text in ReviewPost
okButton = [button1 "OK"]


gradeButtons = DL.intersperse oneSpace $ map gradeButton ['0' .. '9']


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
