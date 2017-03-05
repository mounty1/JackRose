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


import qualified Data.Text as DT (Text, pack)
import Data.Text.Lazy (fromStrict)
import qualified Text.XML as XML


expand :: [DT.Text] -> DT.Text -> [[Maybe String]] -> Either DT.Text [XML.Node]
expand _ _ [] = Left "no data"
expand cols template [list] = if length cols == length list then
		either (const $ Left "cannot parse") (Right . expand' (zip cols list)) (XML.parseText XML.def $ fromStrict template)
	else
		Left $ DT.pack $ "malformed table" ++ show cols ++ "///" ++ show list
-- TODO make this more useful
expand _ _ _ = Left "multiple rows"


-- TODO check for null 'other bits'
expand' :: [(DT.Text, Maybe String)] -> XML.Document -> [XML.Node]
expand' nameValPairs template = (XML.elementNodes $ XML.documentRoot template)
-- [XML.NodeContent $ DT.pack $ fromMaybe "<null field>" $ list !! 5] where
