{-|
Description: HTML formatting of question and answer.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Uses the CSS3 /flex/ solution at <http://stackoverflow.com/questions/90178/make-a-div-fill-the-height-of-the-remaining-screen-space>.
If we ever have to support older browsers, <https://www.google.com.au/search?q=html+fill+rest> such as
<http://stackoverflow.com/questions/21222663/make-nested-div-stretch-to-100-of-remaining-container-div-height/>.
-}


{-# LANGUAGE OverloadedStrings #-}


module PresentHTML (toHTMLdoc, documentHTMLNotice, documentHTML, documentXHTML, gradeButtons, okButton) where


import qualified Yesod.Core as YC
import qualified Foundation (Handler)
import qualified Data.Text as DT (Text, singleton, concat)
import qualified Text.XML as XML
import qualified Text.Blaze.Html as BZH (toHtml)
import qualified Data.Map as DM
import qualified Data.List as DL (intersperse)
import qualified Branding (visibleName)


toHTMLdoc :: XML.Document -> Foundation.Handler YC.Html
toHTMLdoc = YC.liftIO . return . BZH.toHtml


documentHTMLNotice :: DT.Text -> XML.Document
documentHTMLNotice = documentHTML Nothing Branding.visibleName


documentHTML :: Maybe DT.Text -> DT.Text -> DT.Text -> XML.Document
documentHTML styleCSS title content = documentXHTML styleCSS title okButton [makeNode "h1" centreAttr [XML.NodeContent content]]


documentXHTML :: Maybe DT.Text -> DT.Text -> [XML.Node] -> [XML.Node] -> XML.Document
documentXHTML styleCSS title nextButton content = XML.Document
		standardPrologue
		(XML.Element
			(nameXML "html")
			DM.empty
			[makeNode "head"
				[]
				[makeNode "meta"
					[makeAttribute "http-equiv" "Content-Type",
						makeAttribute "content" "application/xhtml;charset=utf-8"]
					[],
				makeNode "title" [] [XML.NodeContent title],
				makeNode "style" [] [XML.NodeContent $ DT.concat $ maybe [ceeSS1, ceeSS2] (\s -> [ceeSS1, s, ceeSS2]) styleCSS ]
				],
			makeNode "body"
				[ makeAttribute "class" "JRbox" ]
				[makeNode "div" [ makeAttribute "class" "JRrow JRcontent" ] content,
				makeNode "table"
					[makeAttribute "class" "JRrow JRfooter"]
					[makeNode "tr"
						[ ]
						[makeButtonDiv [button1 "stats"],
						makeButtonDiv nextButton,
						makeButtonDiv [button1 "logout"]]
					]
				]
			])
		[]


standardPrologue :: XML.Prologue
standardPrologue =
	XML.Prologue
		[XML.MiscInstruction (XML.Instruction "xml" "version=\"1.0\" encoding=\"UTF-8\"")]
		(Just $ XML.Doctype "html" Nothing)
		[]


okButton, gradeButtons :: [XML.Node]


-- label must be synchronised with text in ReviewPost
okButton = [button1 "OK"]


gradeButtons = DL.intersperse oneSpace $ map gradeButton ['0' .. '9']


oneSpace :: XML.Node
oneSpace = XML.NodeContent $ DT.singleton ' '


nameXML :: DT.Text -> XML.Name
nameXML tag = XML.Name tag Nothing Nothing


postAttr, centreAttr :: [(XML.Name, DT.Text)]

postAttr = [makeAttribute "method" "post"]

centreAttr = [makeAttribute "style" "text-align:center;"]


makeButtonDiv :: [XML.Node] -> XML.Node
makeButtonDiv buttons = makeNode "td" centreAttr [makeNode "form" postAttr buttons]


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


ceeSS1, ceeSS2 :: DT.Text
ceeSS1 = "html, body { height: 100%; margin: 0; } .JRbox { display: flex; flex-flow: column; height: 100%; } .JRbox .JRrow { border: 1px dotted grey; } .JRbox .JRrow.JRheader { flex: 0 1 auto; } .JRbox .JRrow.JRcontent { flex: 1 1 auto;"
ceeSS2 = "} .JRbox .JRrow.JRfooter { flex: 0 1 0; width: 100%; }"
