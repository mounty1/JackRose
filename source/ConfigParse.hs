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

module ConfigParse (UserSchema(..),
		SchemaParsing,
		UserDataSources,
		DataSource,
		DataVariant(..),
		View(..),
		Logged(..),
		content) where


import qualified Data.Text as DT (Text, concat, append, singleton, all)
import qualified Data.Text.Read as DTR (decimal)
import qualified Data.List as DL (intersperse)
import qualified Text.XML as XML
import qualified Data.Map as DM
import qualified Data.Maybe as DMy
import qualified Data.Char as DC (isSpace)
import Control.Applicative ((<$>))


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


type UserDataSources = [DataSource]


data DataSource = DataSource DT.Text DT.Text DataVariant


type Attributes = DM.Map XML.Name DT.Text


data DataVariant
		= Postgres { server :: DT.Text, port :: Int, database :: DT.Text, namespace :: Maybe DT.Text, table :: DT.Text }
		| Sqlite3 { tableName :: DT.Text }
		| CSV { separator :: Char, fileCSV :: DT.Text }
		| XMLSource { fileXML :: DT.Text }


data View = View {
		dataSource :: DataSource,
		label :: DT.Text,
		obverse :: [XML.Node],
		backside :: [XML.Node]
	}


data UserSchema = UserSchema {
		sources :: UserDataSources,  -- ^ read-only data sources.
		views :: [View]
	}


type XMLFileContext = [DT.Text]


nullSchema :: Bool -> Logged UserSchema
nullSchema debuggery = Logged [] (if debuggery then Just [] else Nothing) (UserSchema [] [])


-- | Parse the users's configuration file.  This might fail (returning a Left Text)
-- | or succeed (returning a Right UserSchema).
content :: DT.Text -> Bool -> XML.Document -> SchemaParsing

content sourceName debuggery (XML.Document _ top@(XML.Element (XML.Name "jackrose" Nothing Nothing) attrs children) []) =
	DMy.maybe
		(missingAttr context "version")
		checkVersionAttrValue
		(maybeAttrValue "version" attrs) where
		checkVersionAttrValue version =
			if version == documentVersionText then
				contents context children (nullSchema debuggery)
			else
				failToParse context ["version=\"", version, "\" not \"", documentVersionText, "\""]
		context = [ tagText top, "File:" `DT.append` sourceName ]

content sourceName _ (XML.Document _ top@(XML.Element (XML.Name _ _ _) _ _) _) =
	failToParse [ tagText top, "File:" `DT.append` sourceName ] ["document not jackrose"]


maybeAttrValue :: DT.Text -> Attributes -> Maybe DT.Text
maybeAttrValue key attrs = DM.lookup (XML.Name key Nothing Nothing) attrs


maybeIntAttrValue :: DT.Text -> Attributes -> Maybe Int
maybeIntAttrValue key attrs = maybeAttrValue key attrs >>= reduceIt . DTR.decimal


reduceIt :: Either String (Int, DT.Text) -> Maybe Int
reduceIt (Right (n, "")) = Just n
reduceIt _ = Nothing


failToParse :: XMLFileContext -> [DT.Text] -> Either DT.Text a
failToParse context message = Left $ DT.concat stream where
	stream = DL.intersperse ":" (reverse context) ++ (": " : message)


contents :: XMLFileContext -> [XML.Node] -> Logged UserSchema -> SchemaParsing
contents _ [] schema = Right schema
contents context (XML.NodeElement element : xs) schema = schemaItem (tagText element : context) element schema >>= contents context xs
contents context (XML.NodeInstruction _ : xs) schema = contents context xs schema
contents context (XML.NodeComment _ : xs) schema = contents context xs schema
contents context (XML.NodeContent text : xs) schema =
	if DT.all DC.isSpace text then
		contents context xs schema
	else
		failToParse context ["stray content\"", text, "\" in configuration"]


tagText :: XML.Element -> DT.Text
tagText (XML.Element (XML.Name other _ _) _ _) = DT.concat ["<", other, ">"]


schemaItem :: XMLFileContext -> XML.Element -> Logged UserSchema -> SchemaParsing

schemaItem _ (XML.Element (XML.Name "frontispiece" Nothing Nothing) _ _) schema =
	Right schema{warnings = "Unimplemented <frontispiece>" : warnings schema}

-- <source UID="GreekG" name="Greek Grammar" form="Postgres" server="localhost" port="9010" database="learning" namespace="all" table="GreekGrammar">
schemaItem context (XML.Element (XML.Name "source" Nothing Nothing) attrs children) schema =
	attrList context attrs ["UID", "name", "form"]
		>>= \[uid, name, form] -> (dataSourceVariant context form attrs >>= \vary -> Right (DataSource uid name vary))
		>>= oneSource context children schema

schemaItem context (XML.Element (XML.Name other _ _) _ _) _ =
	failToParse context ["Unrecognised schema item <", other, ">"]


oneSource :: XMLFileContext -> [XML.Node] -> Logged UserSchema -> DataSource -> SchemaParsing
oneSource _ [] schema source = Right schema{payload = (payload schema){sources = source : sources (payload schema)}}
oneSource context (XML.NodeElement element : xs) schema source = sourceItem context element schema source >>= \sch -> oneSource context xs sch source
oneSource context (XML.NodeInstruction _ : xs) schema source = oneSource context xs schema source
oneSource context (XML.NodeComment _ : xs) schema source = oneSource context xs schema source
oneSource context (XML.NodeContent text : xs) schema source =
	if DT.all DC.isSpace text then
		oneSource context xs schema source
	else
		failToParse context ["stray content\"", text, "\" in configuration"]


sourceItem :: XMLFileContext -> XML.Element -> Logged UserSchema -> DataSource -> SchemaParsing

sourceItem _ (XML.Element (XML.Name "template" Nothing Nothing) _ _) schema _ =
	Right schema{warnings = "Unimplemented <template>" : warnings schema}

sourceItem _ (XML.Element (XML.Name "invoke" Nothing Nothing) _ _) schema _ =
	Right schema{warnings = "Unimplemented <invoke>" : warnings schema}

sourceItem context (XML.Element (XML.Name "view" Nothing Nothing) attrs children) (Logged w i p) source =
	attrList context attrs ["category"] >>= tryView where
	tryView [category] = viewItem context (Logged w i attrs) (Nothing, Nothing) children
		>>= \(Logged war inf (front, back)) -> Right $ Logged war inf p{views = View source category front back : views p}

sourceItem context (XML.Element (XML.Name other _ _) _ _) _ _ =
	failToParse context ["Unrecognised source item <", other, ">"]


type CardFaces = (Maybe [XML.Node], Maybe [XML.Node])


viewItem :: XMLFileContext -> Logged Attributes -> CardFaces -> [XML.Node] -> ParsingResult ([XML.Node], [XML.Node])

viewItem _ (Logged w i _) (Just frontFace, Just backFace) [] = Right $ Logged w i (frontFace, backFace)
viewItem context lattrs pair (XML.NodeElement element : xs) = viewPart context element pair >>= \nPair -> viewItem context lattrs nPair xs
viewItem context lattrs pair (XML.NodeInstruction _ : xs) = viewItem context lattrs pair xs
viewItem context lattrs pair (XML.NodeComment _ : xs) = viewItem context lattrs pair xs
viewItem context lattrs pair (XML.NodeContent text : xs) =
	if DT.all DC.isSpace text then
		viewItem context lattrs pair xs
	else
		failToParse context ["stray content\"", text, "\" in view"]


viewPart :: XMLFileContext -> XML.Element -> CardFaces -> Either DT.Text CardFaces

viewPart _ (XML.Element (XML.Name "front" Nothing Nothing) attrs children) (Nothing, back) = Right (Just children, back)
viewPart context (XML.Element (XML.Name "front" Nothing Nothing) attrs children) (Just _, back) = failToParse context ["double <front> in view"]
viewPart _ (XML.Element (XML.Name "back" Nothing Nothing) attrs children) (front, Nothing) = Right (front, Just children)
viewPart context (XML.Element (XML.Name "back" Nothing Nothing) attrs children) (Just _, back) = failToParse context ["double <back> in view"]
viewPart context (XML.Element (XML.Name other _ _) _ _) _ = failToParse context ["Unrecognised view item <", other, ">"]


dataSourceVariant :: XMLFileContext -> DT.Text -> Attributes -> Either DT.Text DataVariant

dataSourceVariant context "Postgres" attrs =
	(\[thisServer, thisDatabase, thisTable] -> Postgres thisServer portNo thisDatabase nameSpace thisTable) <$> attrList context attrs [ "server", "database", "table" ] where
	portNo = DMy.fromMaybe 9001 (maybeIntAttrValue "port" attrs)
	nameSpace = maybeAttrValue "namespace" attrs

dataSourceVariant context "SQLite3" attrs =
	(\[ fileName ] -> Sqlite3 fileName) <$> attrList context attrs [ "filename" ]

dataSourceVariant context "CSV" attrs =
	(\[ separator, file ] -> CSV '\t' file) <$> attrList context attrs [ "separator", "file" ]

dataSourceVariant context "XML" attrs =
	(\[ file ] -> XMLSource file) <$> attrList context attrs [ "file" ]

dataSourceVariant context form _ = failToParse context ["Invalid form \"", form, "\""]


-- given a list of attributes, and a list of attribute names, return either:
--	Left "missing attribute"
--	Right $ list of attribute values

attrList :: XMLFileContext -> Attributes -> [DT.Text] -> Either DT.Text [DT.Text]

attrList _ _ [] = Right []

attrList context attrs (attr : rest) =
	DMy.maybe
		(missingAttr context attr)
		mumble
		(maybeAttrValue attr attrs) where
		mumble value = (value :) <$> attrList context attrs rest


missingAttr :: XMLFileContext -> DT.Text -> Either DT.Text a
missingAttr context attrName = failToParse context ["lacking required attribute ", attrName]
