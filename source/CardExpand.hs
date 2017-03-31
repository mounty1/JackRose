{-|
Description: Take data names, values etc. and return an XHTML document for display
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

{-# LANGUAGE OverloadedStrings #-}


module CardExpand (expand) where


import qualified Data.Text as DT (Text, pack, concat, empty)
import qualified Data.Map as DM (lookup)
import qualified Data.List as DL (lookup)
import Data.Text.Lazy (fromChunks)
import qualified Text.XML as XML
import GHC.Exception (SomeException)


-- | Given a list of column names and value, convert the Text template to XML and substitute in
-- parameter values.
expand :: [DT.Text] -> Maybe DT.Text -> DT.Text -> [[Maybe String]] -> Either DT.Text [XML.Node]
expand _ _ _ [] = Left "no data"
expand cols maybeFront template [list] = if length cols /= length list then
		Left $ DT.pack $ "malformed table:" ++ show tableCols ++ " != " ++ show rowCols
	else
		either (Left . DT.pack . show) (expand' (zip cols list) maybeFront) (parseTemplate template) where
		tableCols = length cols
		rowCols = length list
-- TODO make this more useful
expand _ _ _ _ = Left "multiple rows"


parseTemplate :: DT.Text -> Either SomeException XML.Document
parseTemplate template = XML.parseText XML.def $ fromChunks ["<div>", template, "</div>"]


expand' :: [(DT.Text, Maybe String)] -> Maybe DT.Text -> XML.Document -> Either DT.Text [XML.Node]
expand' nameValPairs maybeFront template = fmap (map (substitute (nameValPairs, maybeFront))) (extractMeat template)


-- Recursive substitution of <field name="colname"/> and <frontSide/> nodes.
-- Anything else is passed through verbatim.
substitute ::  ([(DT.Text, Maybe String)], Maybe DT.Text) -> XML.Node -> XML.Node

substitute (pairs, _) n@(XML.NodeElement (XML.Element (XML.Name "field" Nothing Nothing) attrs [])) = maybe n fieldNameValue (DM.lookup "name" attrs) where
		fieldNameValue fldName = XML.NodeContent $ maybe
				(DT.concat ["no field ", fldName])
				(maybe DT.empty DT.pack)
				(DL.lookup fldName pairs)

substitute (pairs, maybeFront) (XML.NodeElement (XML.Element (XML.Name "frontSide" Nothing Nothing) attrs [])) =
	maybe
		(XML.NodeContent "?? frontside ??")
		(either (XML.NodeContent . DT.pack . show) (either XML.NodeContent expandFully . extractMeat) . parseTemplate)
		maybeFront
	where expandFully = XML.NodeElement . XML.Element "span" attrs . map (substitute (pairs, Nothing))

substitute parts (XML.NodeElement (XML.Element element attrs subNodes)) =
	XML.NodeElement (XML.Element element attrs (map (substitute parts) subNodes))

substitute _ n = n


-- XML.parseText produces a complete document;  extract the bit we want
-- TODO check for null _map
extractMeat :: XML.Document -> Either DT.Text [XML.Node]
extractMeat (XML.Document (XML.Prologue [] Nothing []) (XML.Element (XML.Name "div" Nothing Nothing) _map nodes) []) = Right nodes
-- since we created the document, any other form is unexpected
extractMeat _ = Left "malformed document"
