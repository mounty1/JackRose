{-|
Description: Unpick user configuration XML and return it.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

Take the users's configuration file and pick out the data sources and other information.
If an unrecoverable error occurs, return a text diagnostic in the Left constructor;
otherwise, return an object of the schema data, with a list of warnings and a list of
informational messages.  The informational messages include debugging messages if the
debugging option is passed in.
-}


{-# LANGUAGE OverloadedStrings #-}


module ConfigParse (UserSchema(..), SchemaParsing, View(..), Logged(..), content) where


import qualified Data.Text as DT (Text, concat, append, singleton, all, length)
import qualified Data.Text.Read as DTR (decimal)
import qualified Data.List as DL (intersperse)
import qualified Text.XML as XML
import qualified Data.Map as DM
import qualified Data.Maybe as DMy
import qualified Data.Char as DC (isSpace)
import Control.Applicative ((<$>), (<*>))
import qualified DataSource


-- | Version of the configuration file schema.  This is incremented only
-- | when a breaking change is made to the format.
documentVersion :: Int
documentVersion = 1


-- | Textual representation of schema version number.
documentVersionText :: DT.Text
documentVersionText = DT.singleton $ toEnum (48 + documentVersion)


data Logged a = Logged {
		warnings :: [DT.Text], -- ^ tell the user about these and log them.
		info :: Maybe [DT.Text],  -- ^ just log these, if info-level requested.
		payload :: a
	}


type ParsingResult a = Either DT.Text (Logged a)


type SchemaParsing = ParsingResult UserSchema


type Attributes = DM.Map XML.Name DT.Text


data View = View {
		dataSource :: DataSource.DataSource,
		label :: DT.Text,
		obverse :: [XML.Node],
		backside :: [XML.Node]
	}


data UserSchema = UserSchema {
		sources :: [DataSource.DataSource],  -- ^ read-only data sources.
		views :: [View]
	}


type XMLFileContext = [DT.Text]


-- | Parse the users's configuration file.  This might fail (returning a @Left DT.Text@)
-- or succeed (returning a @Right UserSchema@).
content :: DT.Text -> Bool -> XML.Document -> SchemaParsing

content sourceName debuggery (XML.Document _ top@(XML.Element (XML.Name "jackrose" Nothing Nothing) attrs children) []) =
	attrList context attrs [ "version" ] >>= checkVersionAttrValue where
		checkVersionAttrValue [version] =
			if version == documentVersionText then
				contents context children nullSchema
			else
				failToParse context ["version=\"", version, "\" not \"", documentVersionText, "\""]
		context = [ tagText top, "File=" `DT.append` sourceName ]
		nullSchema = Logged [] (if debuggery then Just [] else Nothing) (UserSchema [] [])

content sourceName _ (XML.Document _ top@(XML.Element (XML.Name _ _ _) _ _) _) =
	failToParse [ tagText top, "File=" `DT.append` sourceName ] ["document not jackrose"]


-- | Try to retrieve an attribute value from a list.
maybeAttrValue :: DT.Text -> Attributes -> Maybe DT.Text
maybeAttrValue key attrs = DM.lookup (XML.Name key Nothing Nothing) attrs


-- | Try to retrive an integer value from an attribute list.
-- Fail if the attribute doesn't exist or isn't a number representation.
maybeIntAttrValue :: DT.Text -> Attributes -> Maybe Int
maybeIntAttrValue key attrs = maybeAttrValue key attrs >>= reduceIt . DTR.decimal


reduceIt :: Either String (Int, DT.Text) -> Maybe Int
reduceIt (Right (n, "")) = Just n
reduceIt _ = Nothing


-- | Fail to parse.  This is a quite crucial function because it returns a Left value;
-- thus, all the @<$>@ and @>>=@ will just pass through the failure message and eventually
-- return it to the caller.
failToParse :: XMLFileContext -> [DT.Text] -> Either DT.Text a
failToParse context message = Left $ DT.concat stream where
	stream = DL.intersperse ":" (reverse context) ++ (": " : message)


-- | Parse nodes directly within the top-level @<jackrose>@ document.
contents :: XMLFileContext -> [XML.Node] -> Logged UserSchema -> SchemaParsing
contents _ [] schema = Right schema
contents context (XML.NodeElement element : xs) schema = schemaItem (tagText element : context) element schema >>= contents context xs
contents context (XML.NodeInstruction _ : xs) schema = contents context xs schema
contents context (XML.NodeComment _ : xs) schema = contents context xs schema
contents context (XML.NodeContent text : xs) schema =
	if DT.all DC.isSpace text then
		contents context xs schema
	else
		failToParse context invalidItem


-- | show Element
tagText :: XML.Element -> DT.Text
tagText (XML.Element (XML.Name plainName namespace prefix) attrs _) = DT.concat $ ["<", qualiform namespace, qualiform prefix, plainName] ++ map attrForm (DM.toList attrs) ++ [">"]


-- | show attribute+value pair
attrForm :: (XML.Name, DT.Text) -> DT.Text
attrForm (XML.Name plainName namespace prefix, attrValue) = DT.concat [" ", qualiform namespace, qualiform prefix, plainName, "=\"", attrValue, "\""]


-- show namespace
qualiform :: Maybe DT.Text -> DT.Text
qualiform Nothing = ""
qualiform (Just repr) = repr `DT.append` ":"


-- | Parse @<tag>@s directly within the top-level @<jackrose>@ document.
schemaItem :: XMLFileContext -> XML.Element -> Logged UserSchema -> SchemaParsing

schemaItem _ (XML.Element (XML.Name "frontispiece" Nothing Nothing) _ _) schema =
	Right schema{warnings = "Unimplemented <frontispiece>" : warnings schema}

-- | @<source UID="GreekG" name="Greek Grammar" form="Postgres" server="localhost" port="9010" database="learning" namespace="all" table="GreekGrammar">@
-- Pick out the attributes to work out what class of data source then pass on to <view> etc. parsing
schemaItem context (XML.Element (XML.Name "source" Nothing Nothing) attrs children) schema =
	attrList context attrs ["UID", "name", "form"]
		>>= formDataSource
		>>= oneSource context children schema where
		formDataSource [uid, name, form] = DataSource.DataSource uid name <$> dataSourceVariant context form attrs

schemaItem context (XML.Element (XML.Name other _ _) _ _) _ =
	failToParse context invalidItem


-- | Parse the nodes directly within a @<source>@
oneSource :: XMLFileContext -> [XML.Node] -> Logged UserSchema -> DataSource.DataSource -> SchemaParsing
oneSource _ [] schema source = Right schema{payload = (payload schema){sources = source : sources (payload schema)}}
oneSource context (XML.NodeElement element : xs) schema source = sourceItem (tagText element : context) element schema source >>= \sch -> oneSource context xs sch source
oneSource context (XML.NodeInstruction _ : xs) schema source = oneSource context xs schema source
oneSource context (XML.NodeComment _ : xs) schema source = oneSource context xs schema source
oneSource context (XML.NodeContent text : xs) schema source =
	if DT.all DC.isSpace text then
		oneSource context xs schema source
	else
		failToParse context invalidItem


-- | Parse @<tag>@s directly within a @<source>@
sourceItem :: XMLFileContext -> XML.Element -> Logged UserSchema -> DataSource.DataSource -> SchemaParsing

sourceItem _ (XML.Element (XML.Name "template" Nothing Nothing) _ _) schema _ =
	Right schema{warnings = "Unimplemented <template>" : warnings schema}

sourceItem _ (XML.Element (XML.Name "invoke" Nothing Nothing) _ _) schema _ =
	Right schema{warnings = "Unimplemented <invoke>" : warnings schema}

sourceItem context (XML.Element (XML.Name "view" Nothing Nothing) attrs children) (Logged w i p) source =
	attrList context attrs ["category"] >>= tryView where
	tryView [category] = spliceView category <$> viewItem context (Logged w i attrs) (Nothing, Nothing) children
	spliceView category (Logged war inf (front, back)) = Logged war inf p{views = View source category front back : views p}

sourceItem context (XML.Element (XML.Name other _ _) _ _) _ _ =
	failToParse context invalidItem


type CardFaces = (Maybe [XML.Node], Maybe [XML.Node])


-- | Parse nodes within a @<view>@
viewItem :: XMLFileContext -> Logged Attributes -> CardFaces -> [XML.Node] -> ParsingResult ([XML.Node], [XML.Node])

viewItem _ (Logged w i _) (Just frontFace, Just backFace) [] = Right $ Logged w i (frontFace, backFace)
viewItem context lattrs pair (XML.NodeElement element : xs) = viewPart (tagText element : context) element pair >>= \nPair -> viewItem context lattrs nPair xs
viewItem context lattrs pair (XML.NodeInstruction _ : xs) = viewItem context lattrs pair xs
viewItem context lattrs pair (XML.NodeComment _ : xs) = viewItem context lattrs pair xs
viewItem context lattrs pair (XML.NodeContent text : xs) =
	if DT.all DC.isSpace text then
		viewItem context lattrs pair xs
	else
		failToParse context invalidItem


dupInView, extraAttributes, invalidItem :: [ DT.Text ]
dupInView = ["duplicate in view"]
extraAttributes = ["attributes given"]
invalidItem = ["invalid item"]


-- | Parse @<tag>@s within a @<view>@;  there should be just one each of @<front>@ and @<back>@
viewPart :: XMLFileContext -> XML.Element -> CardFaces -> Either DT.Text CardFaces

viewPart context (XML.Element (XML.Name "front" Nothing Nothing) attrs children) (Nothing, back) =
	if DM.null attrs then Right (Just children, back) else failToParse context extraAttributes

viewPart context (XML.Element (XML.Name "front" Nothing Nothing) _ _) (Just _, back) = failToParse context dupInView

viewPart context (XML.Element (XML.Name "back" Nothing Nothing) attrs children) (front, Nothing) =
	if DM.null attrs then Right (front, Just children) else failToParse context extraAttributes

viewPart context (XML.Element (XML.Name "back" Nothing Nothing) _ _) (Just _, back) = failToParse context dupInView

viewPart context (XML.Element (XML.Name other _ _) _ _) _ = failToParse context invalidItem


-- | Determine data source class, specified entirely in attributes of <source>
dataSourceVariant :: XMLFileContext -> DT.Text -> Attributes -> Either DT.Text DataSource.DataVariant

dataSourceVariant context "Postgres" attrs =
	(\[thisServer, thisDatabase, thisTable] -> DataSource.Postgres thisServer portNo thisDatabase nameSpace thisTable thisUser thisPass) <$> attrList context attrs [ "server", "database", "table" ] where
	portNo = maybeIntAttrValue "port" attrs
	nameSpace = maybeAttrValue "namespace" attrs
	thisUser = maybeAttrValue "username" attrs
	thisPass = maybeAttrValue "password" attrs

dataSourceVariant context "SQLite3" attrs =
	(\[ fileName ] -> DataSource.Sqlite3 fileName) <$> attrList context attrs [ "filename" ]

dataSourceVariant context "CSV" attrs =
	attrList context attrs [ "separator", "file" ] >>= seeEssVee where
	seeEssVee [ separator, file ]
		| DT.length separator == 1 = Right $ DataSource.CSV '\t' file
		| otherwise = failToParse context [ "CSV separator \"", separator, "\" must be one character" ]

dataSourceVariant context "XML" attrs =
	(\[ file ] -> DataSource.XMLSource file) <$> attrList context attrs [ "file" ]

dataSourceVariant context form _ = failToParse context ["Invalid form \"", form, "\""]


-- | given a list of attributes, and a list of attribute names, return either:
-- | * @Left "missing attribute"@
-- | * @Right $ list of attribute values@
attrList :: XMLFileContext -> Attributes -> [DT.Text] -> Either DT.Text [DT.Text]

attrList _ _ [] = Right []

attrList context attrs (attr : rest) =
	DMy.maybe
		(failToParse context ["missing attribute ", attr])
		mumble
		(maybeAttrValue attr attrs) where
		mumble value = (value :) <$> attrList context attrs rest
