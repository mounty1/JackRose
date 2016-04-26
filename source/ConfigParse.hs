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


module ConfigParse (UserSchema(..), SchemaParsing, View(..), content) where


import qualified Data.Text as DT (Text, concat, append, singleton, all, length, head)
import qualified Data.Text.Read as DTR (decimal)
import qualified Data.List as DL (intersperse, null, (\\))
import qualified Text.XML as XML
import qualified Data.Map as DM
import qualified Data.Char as DC (isSpace)
-- import Control.Applicative ((<$>), (<*>))
import qualified DataSource
import Data.Maybe (fromJust)
import Data.Either (partitionEithers)
import Control.Monad.Logger (LoggingT, logWarnNS, logDebugNS, logErrorNS)


-- | Version of the configuration file schema.  This is incremented only
-- | when a breaking change is made to the format.
documentVersion :: Int
documentVersion = 1


-- Textual representation of schema version number.
documentVersionText :: DT.Text
documentVersionText = DT.singleton $ toEnum (48 + documentVersion)


newtype ParsingResult a = ParsingResult (LoggingT IO (Either DT.Text a))


instance Applicative ParsingResult where
	pure = ParsingResult . return . Right
	ParsingResult a <*> ParsingResult b = ParsingResult $ a >>= \f -> b >>= \x -> return (f <*> x)


instance Monad ParsingResult where
	return = ParsingResult . return . Right
	(>>=) (ParsingResult v) f = ParsingResult (v >>= either (return . Left) richtig) where
		richtig r = emm where (ParsingResult emm) = f r


instance Functor ParsingResult where
	fmap f (ParsingResult v) = ParsingResult $ fmap (fmap f) v


type SchemaParsing = ParsingResult UserSchema


type Attributes = DM.Map XML.Name DT.Text


data View = View {
		dataSource :: DataSource.DataSource,
		label :: DT.Text,
		obverse :: [XML.Node],
		backside :: [XML.Node]
	}


newtype UserSchema = UserSchema [View]


type XMLFileContext = [DT.Text]


logSource :: DT.Text
logSource = "user-schema"


-- | Parse the users's configuration file.  This might fail (returning a @Left DT.Text@)
-- | or succeed (returning a @Right UserSchema@).
content :: DT.Text -> XML.Document -> LoggingT IO (Either DT.Text UserSchema)

content sourceName doco = result where (ParsingResult result) = content' sourceName doco


-- Internal version of the above;  no need to expose the newtype wrapper,
-- which exists only to allow for instance declarations.
content' :: DT.Text -> XML.Document -> SchemaParsing

content' sourceName (XML.Document _ top@(XML.Element (XML.Name "jackrose" Nothing Nothing) attrs children) []) =
	attrList context attrs ["version"] [] >>= checkVersionAttrValue where
		checkVersionAttrValue ([version], []) =
			if version == documentVersionText then
				contents context children (UserSchema [])
			else
				failToParse context ["version=\"", version, "\" not \"", documentVersionText, "\""]
		context = [ tagText top, "File=" `DT.append` sourceName ]

content' sourceName (XML.Document _ top@(XML.Element (XML.Name _ _ _) _ _) _) =
	failToParse [ tagText top, "File=" `DT.append` sourceName ] ["document not jackrose"]


-- Try to convert an attribute value to an integer;
-- the attribute may not exist or may not be a number representation.
maybeIntAttrValue :: DT.Text -> Maybe Int
maybeIntAttrValue = reduceIt . DTR.decimal


reduceIt :: Either String (Int, DT.Text) -> Maybe Int
reduceIt (Right (n, "")) = Just n
reduceIt _ = Nothing


-- Fail to parse.  This is a quite crucial function because it returns a Left value;
-- thus, all the @<$>@ and @>>=@ will just pass through the failure message and eventually
-- return it to the caller.
failToParse :: XMLFileContext -> [DT.Text] -> ParsingResult a
failToParse context message = ParsingResult $ logErrorNS logSource stream >> (return $ Left $ DT.concat message) where
	stream = DT.concat $ DL.intersperse ":" (reverse context) ++ (": " : message)


failAllButBlank :: XMLFileContext -> DT.Text -> a -> ParsingResult a
failAllButBlank context text allow = if DT.all DC.isSpace text then return allow else failToParse context invalidItem


-- Parse nodes directly within the top-level @<jackrose>@ document.
contents :: XMLFileContext -> [XML.Node] -> UserSchema -> SchemaParsing
contents _ [] win = return win
contents context (XML.NodeElement element : xs) win = schemaItem (tagText element : context) element win >>= contents context xs
contents context (XML.NodeInstruction _ : xs) win = contents context xs win
contents context (XML.NodeComment _ : xs) win = contents context xs win
contents context (XML.NodeContent text : xs) win = contents context xs win >>= failAllButBlank context text


-- show Element
tagText :: XML.Element -> DT.Text
tagText (XML.Element (XML.Name plainName namespace prefix) attrs _) = DT.concat $ ["<", qualiform namespace, qualiform prefix, plainName] ++ map attrForm (DM.toList attrs) ++ [">"]


-- show attribute+value pair
attrForm :: (XML.Name, DT.Text) -> DT.Text
attrForm (XML.Name plainName namespace prefix, attrValue) = DT.concat [" ", qualiform namespace, qualiform prefix, plainName, "=\"", attrValue, "\""]


-- show namespace
qualiform :: Maybe DT.Text -> DT.Text
qualiform Nothing = ""
qualiform (Just repr) = repr `DT.append` ":"


-- Parse @<tag>@s directly within the top-level @<jackrose>@ document.
schemaItem :: XMLFileContext -> XML.Element -> UserSchema -> SchemaParsing

schemaItem _ (XML.Element (XML.Name "frontispiece" Nothing Nothing) _ _) schema =
	ParsingResult $ logWarnNS logSource "Unimplemented <frontispiece>"  >> (return $ Right schema)

-- @<source UID="GreekG" name="Greek Grammar" form="Postgres" server="localhost" port="9010" database="learning" namespace="all" table="GreekGrammar">@
-- Pick out the attributes to work out what class of data source then pass on to <view> etc. parsing
schemaItem context (XML.Element (XML.Name "source" Nothing Nothing) attrs children) schema =
	attrList context attrs ["UID", "name", "form"] []
		>>= (\([uid, name, form], []) -> dataSourceVariant context form attrs >>= (\zee -> return $ DataSource.DataSource uid name zee))
		>>= oneSource context children schema where

schemaItem context (XML.Element (XML.Name other _ _) _ _) _ = failToParse context (other : ": " : invalidItem)


-- Parse the nodes directly within a @<source>@
oneSource :: XMLFileContext -> [XML.Node] -> UserSchema -> DataSource.DataSource -> SchemaParsing
oneSource _ [] schema _ = return schema
oneSource context (XML.NodeElement element : xs) schema source = sourceItem (tagText element : context) element schema source >>= \sch -> oneSource context xs sch source
oneSource context (XML.NodeInstruction _ : xs) schema source = oneSource context xs schema source
oneSource context (XML.NodeComment _ : xs) schema source = oneSource context xs schema source
oneSource context (XML.NodeContent text : xs) schema source = oneSource context xs schema source >>= failAllButBlank context text


-- Parse @<tag>@s directly within a @<source>@
sourceItem :: XMLFileContext -> XML.Element -> UserSchema -> DataSource.DataSource -> SchemaParsing

sourceItem _ (XML.Element (XML.Name "template" Nothing Nothing) _ _) schema _ =
	ParsingResult $ logWarnNS logSource "Unimplemented <template>"  >> (return $ Right schema)

sourceItem _ (XML.Element (XML.Name "invoke" Nothing Nothing) _ _) schema _ =
	ParsingResult $ logWarnNS logSource "Unimplemented <invoke>"  >> (return $ Right schema)

sourceItem context (XML.Element (XML.Name "view" Nothing Nothing) attrs children) (UserSchema p) source =
	attrList context attrs ["category"] []
		>>= \([category], []) -> spliceView category `fmap` viewItem context attrs (Nothing, Nothing) children where
	spliceView category (front, back) = UserSchema (View source category front back : p)

sourceItem context (XML.Element (XML.Name other _ _) _ _) _ _ = failToParse context (other : ": " : invalidItem)


type CardFaces = (Maybe [XML.Node], Maybe [XML.Node])


-- Parse nodes within a @<view>@
viewItem :: XMLFileContext -> Attributes -> CardFaces -> [XML.Node] -> ParsingResult ([XML.Node], [XML.Node])

viewItem _ _ (Just frontFace, Just backFace) [] = return (frontFace, backFace)
viewItem context lattrs pair (XML.NodeElement element : xs) = viewPart (tagText element : context) element pair >>= \nPair -> viewItem context lattrs nPair xs
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


-- Determine data source class, specified entirely in attributes of <source>
dataSourceVariant :: XMLFileContext -> DT.Text -> Attributes -> ParsingResult DataSource.DataVariant

dataSourceVariant context "Postgres" attrs =
	(\([thisServer, thisDatabase, thisTable], [portRepr, nameSpace, thisUser, thisPass]) ->
			DataSource.Postgres thisServer (portRepr >>= maybeIntAttrValue) thisDatabase nameSpace thisTable thisUser thisPass) `fmap`
		attrList context attrs [ "server", "database", "table" ] ["port", "namespace", "username", "password"]

dataSourceVariant context "SQLite3" attrs =
	(\([fileName], []) -> DataSource.Sqlite3 fileName) `fmap` attrList context attrs ["filename"] []

dataSourceVariant context "CSV" attrs =
	attrList context attrs ["separator", "file"] [] >>= seeEssVee where
	seeEssVee ([separator, file], [])
		| DT.length separator == 1 = return $ DataSource.CSV (DT.head separator) file
		| otherwise = failToParse context [ "CSV separator \"", separator, "\" must be one character" ]

dataSourceVariant context "XML" attrs =
	(\([file], []) -> DataSource.XMLSource file) `fmap` attrList context attrs ["file"] []

dataSourceVariant context form _ = failToParse context ["Invalid form \"", form, "\""]


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

	-- Try to retrieve an attribute value from a list.
	maybeAttrValue :: DT.Text -> Maybe DT.Text
	maybeAttrValue key = DM.lookup (XML.Name key Nothing Nothing) attrMap


mkAttrDebugPair :: (DT.Text, DT.Text) -> DT.Text
mkAttrDebugPair (key, value) = DT.concat ["attribute: ", key, "=\"", value, "\""]


mkOptAttrDebugPair :: (DT.Text, Maybe DT.Text) -> DT.Text
mkOptAttrDebugPair (key, Nothing) = DT.concat ["attribute: ", key, " not found"]
mkOptAttrDebugPair (key, Just value) = mkAttrDebugPair (key, value)
