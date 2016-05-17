{-|
Description: Unpick user configuration XML and return it.
Copyright: (c) Michael Mounteney, 2015
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


{-# LANGUAGE OverloadedStrings #-}


module ConfigParse (SchemaParsing, content) where


import qualified Data.Text as DT (Text, concat, append, singleton, all, toLower)
import qualified Data.List as DL (intersperse, null, (\\))
import qualified Text.XML as XML
import qualified Data.Map as DM
import qualified Data.Char as DC (isSpace)
import qualified JRState
import Data.Maybe (fromJust)
import Data.Either (partitionEithers)
import Control.Monad.Logger (LoggingT, logWarnNS, logDebugNS, logErrorNS)
import MaybeIntValue (maybeIntValue)
import qualified ConfigData (UserSchemaCpt(..), UserSchema)


-- | Version of the configuration file schema.  This is incremented only
-- | when a breaking change is made to the format.
documentVersion :: Int
documentVersion = 1


-- Textual representation of schema version number.
documentVersionText :: DT.Text
documentVersionText = DT.singleton $ toEnum (48 + documentVersion)


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


type SchemaParsing = ParsingResult ConfigData.UserSchema


type Attributes = DM.Map XML.Name DT.Text


data XMLFileContext = XMLFileContext {
	nodes :: [DT.Text],
	views :: [DT.Text],
	shuffle :: Bool,
	dataSchemes :: JRState.DataSchemes
}


logSource :: DT.Text
logSource = "user-schema"


-- | Parse the users's configuration file.  This might fail (returning a @Left DT.Text@)
-- | or succeed (returning a @Right ConfigData.UserSchema@).
content :: JRState.JRState -> DT.Text -> XML.Document -> IO (Either DT.Text ConfigData.UserSchema)

content site sourceName doco = JRState.getDataSchemes site >>= JRState.runFilteredLoggingT site . unwrapPR . content' sourceName doco site


-- Internal version of the above;  no need to expose the newtype wrapper,
-- which exists only to allow for instance declarations.
content' :: DT.Text -> XML.Document -> JRState.JRState -> JRState.DataSchemes -> SchemaParsing

content' sourceName (XML.Document _ top@(XML.Element (XML.Name "jackrose" Nothing Nothing) attrs children) []) site dSchemes =
	attrList context attrs ["version"] []
		>>= \([version], []) ->
			if version == documentVersionText then
				contents context children []
			else
				failToParse context ["version=\"", version, "\" not \"", documentVersionText, "\""] where
	context = mkContext top sourceName (JRState.shuffleCards site) dSchemes

content' sourceName (XML.Document _ top@(XML.Element (XML.Name _ _ _) _ _) _) _ dSchemes =
	failToParse (mkContext top sourceName False dSchemes) ["document not jackrose"]


mkContext :: XML.Element -> DT.Text -> Bool -> JRState.DataSchemes -> XMLFileContext
mkContext top sourceName = XMLFileContext [ tagText top, "File=" `DT.append` sourceName ] []


-- Fail to parse.  This is a quite crucial function because it returns a Left value;
-- thus, all the @<$>@ and @>>=@ will just pass through the failure message and eventually
-- return it to the caller.
failToParse :: XMLFileContext -> [DT.Text] -> ParsingResult a
failToParse context message = ParsingResult $ logErrorNS logSource stream >> (return $ Left $ DT.concat message) where
	stream = DT.concat $ DL.intersperse ":" (reverse $ nodes context) ++ (": " : message)


failAllButBlank :: XMLFileContext -> DT.Text -> a -> ParsingResult a
failAllButBlank context text allow = if DT.all DC.isSpace text then return allow else failToParse context invalidItem


-- show Element
tagText :: XML.Element -> DT.Text
tagText (XML.Element (XML.Name plainName namespace prefix) attrs _) = DT.concat $ ["<", qualiform namespace, qualiform prefix, plainName] ++ map attrForm (DM.toList attrs) ++ [">"]


-- Add extra level to XML nodeStack
tagStack :: XMLFileContext -> XML.Element -> XMLFileContext
tagStack context element = context{nodes = tagText element : nodes context}


-- show attribute+value pair
attrForm :: (XML.Name, DT.Text) -> DT.Text
attrForm (XML.Name plainName namespace prefix, attrValue) = DT.concat [" ", qualiform namespace, qualiform prefix, plainName, "=\"", attrValue, "\""]


-- show namespace
qualiform :: Maybe DT.Text -> DT.Text
qualiform Nothing = ""
qualiform (Just repr) = repr `DT.append` ":"


reshuffle :: Bool -> Maybe DT.Text -> Bool
reshuffle now Nothing = now
reshuffle now (Just text) =
	if lText `elem` ["y", "yes", "1", "t", "true" ] then
		True
	else if lText `elem` ["n", "no", "0", "f", "false"] then
		False
	else
		now where
	lText = DT.toLower text


-- Parse nodes directly within the top-level @<jackrose>@ document.
contents :: XMLFileContext -> [XML.Node] -> ConfigData.UserSchema -> SchemaParsing
contents _ [] win = return win
contents context (XML.NodeElement element : xs) win = schemaItem (tagStack context element) element win >>= contents context xs
contents context (XML.NodeInstruction _ : xs) win = contents context xs win
contents context (XML.NodeComment _ : xs) win = contents context xs win
contents context (XML.NodeContent text : xs) win = contents context xs win >>= failAllButBlank context text


-- Parse @<tag>@s directly within the top-level @<jackrose>@ document.
schemaItem :: XMLFileContext -> XML.Element -> ConfigData.UserSchema -> SchemaParsing

schemaItem _ (XML.Element (XML.Name "frontispiece" Nothing Nothing) _ _) schema =
	ParsingResult $ logWarnNS logSource "Unimplemented <frontispiece>"  >> (return $ Right schema)

schemaItem _ (XML.Element (XML.Name "template" Nothing Nothing) _ _) schema =
	ParsingResult $ logWarnNS logSource "Unimplemented <template>"  >> (return $ Right schema)

schemaItem _ (XML.Element (XML.Name "invoke" Nothing Nothing) _ _) schema =
	ParsingResult $ logWarnNS logSource "Unimplemented <invoke>"  >> (return $ Right schema)

schemaItem context (XML.Element (XML.Name "deck" Nothing Nothing) attrs children) schema =
	attrList context attrs ["name"] ["limit", "shuffle"]
		>>= \([name], [maybeLimit, maybeShuffle]) -> (\ss -> ConfigData.SubSchema (maybeLimit >>= maybeIntValue) (reshuffle (shuffle context) maybeShuffle) name ss : schema) `fmap` contents context{views = name : views context} children []

schemaItem context (XML.Element (XML.Name "view" Nothing Nothing) attrs children) schema =
	attrList context attrs ["UID", "source"] ["limit", "shuffle"]
		>>= \([idy, source], [maybeLimit, maybeShuffle]) -> maybe
				(failToParse context (source : ": " : invalidItem))
				(\conn -> 
					(\(front, back) -> ConfigData.View conn (maybeLimit >>= maybeIntValue) (reshuffle (shuffle context) maybeShuffle) idy front back : schema)
						`fmap` viewItem context attrs (Nothing, Nothing) children)
				(DM.lookup source (dataSchemes context)) where

schemaItem context (XML.Element (XML.Name other _ _) _ _) _ = failToParse context (other : ": " : invalidItem)


type CardFaces = (Maybe [XML.Node], Maybe [XML.Node])


-- Parse nodes within a @<view>@
viewItem :: XMLFileContext -> Attributes -> CardFaces -> [XML.Node] -> ParsingResult ([XML.Node], [XML.Node])

viewItem _ _ (Just frontFace, Just backFace) [] = return (frontFace, backFace)
viewItem context lattrs pair (XML.NodeElement element : xs) = viewPart (tagStack context element) element pair >>= \nPair -> viewItem context lattrs nPair xs
viewItem context lattrs pair (XML.NodeInstruction _ : xs) = viewItem context lattrs pair xs
viewItem context lattrs pair (XML.NodeComment _ : xs) = viewItem context lattrs pair xs
viewItem context lattrs pair (XML.NodeContent text : xs) = viewItem context lattrs pair xs >>= failAllButBlank context text
viewItem context _ _ [] = failToParse context invalidItem


dupInView, extraAttributes, invalidItem :: [DT.Text]
dupInView = ["duplicate in view"]
extraAttributes = ["attributes given"]
invalidItem = ["invalid item"]


-- Parse @<tag>@s within a @<view>@;  there should be just one each of @<front>@ and @<back>@
viewPart :: XMLFileContext -> XML.Element -> CardFaces -> ParsingResult CardFaces

viewPart context (XML.Element (XML.Name "front" Nothing Nothing) attrs children) (Nothing, back) =
	if DM.null attrs then return (Just children, back) else failToParse context extraAttributes

viewPart context (XML.Element (XML.Name "front" Nothing Nothing) _ _) (Just _, _) = failToParse context dupInView

viewPart context (XML.Element (XML.Name "back" Nothing Nothing) attrs children) (front, Nothing) =
	if DM.null attrs then return (front, Just children) else failToParse context extraAttributes

viewPart context (XML.Element (XML.Name "back" Nothing Nothing) _ _) (Just _, _) = failToParse context dupInView

viewPart context (XML.Element (XML.Name other _ _) _ _) _ = failToParse context (other : ": " : invalidItem)


-- given a list of attributes, and a list of attribute names, return either:
-- * @Left "missing attribute"@
-- * @Right $ list of attribute values@
attrList :: XMLFileContext -> Attributes -> [DT.Text] -> [DT.Text] -> ParsingResult ([DT.Text], [Maybe DT.Text])

attrList context attrMap attrRequired attrOptional =
	if DL.null missingRequired then
		ParsingResult
			-- this first zip only works when all required key values are found
			$ mapM_ (logDebugNS logSource . mkAttrDebugPair) (zip attrRequired foundRequired)
			>> mapM_ (logDebugNS logSource . mkOptAttrDebugPair) (zip attrOptional foundOptional)
			>> mapM_ (logWarnNS logSource . mkStrayAttrPair) strayKeys
			>> (return $ Right (foundRequired, foundOptional))
	else
		failToParse context ("missing attribute(s): " : DL.intersperse "," missingRequired) where
	foundOptional = map maybeAttrValue attrOptional
	lookupOneAttr attr = maybe (Left attr) Right $ maybeAttrValue attr
	(missingRequired, foundRequired) = partitionEithers $ map lookupOneAttr attrRequired
	strayKeys = (map (\(XML.Name name Nothing Nothing) -> name) (DM.keys attrMap) DL.\\ attrRequired) DL.\\ attrOptional
	mkStrayAttrPair key = DT.concat ["unused attribute: ", key, "=\"", fromJust (maybeAttrValue key), "\""]
	maybeAttrValue key = DM.lookup (XML.Name key Nothing Nothing) attrMap


mkAttrDebugPair :: (DT.Text, DT.Text) -> DT.Text
mkAttrDebugPair (key, value) = DT.concat ["attribute: ", key, "=\"", value, "\""]


mkOptAttrDebugPair :: (DT.Text, Maybe DT.Text) -> DT.Text
mkOptAttrDebugPair (key, Nothing) = DT.concat ["attribute: ", key, " not found"]
mkOptAttrDebugPair (key, Just value) = mkAttrDebugPair (key, value)
