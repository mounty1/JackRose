{-|
Description: Pack multiple Text items into one persistable item, without loss
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module TextList (enSerialise, deSerialise) where


import qualified Data.Text as DT (splitOn, Text, pack, replace, singleton, concat)
import Data.List (intersperse, foldl')
import Data.Tuple (swap)


enSerialise :: [DT.Text] -> DT.Text
enSerialise = DT.concat . intersperse fs . map escapeText


deSerialise :: DT.Text -> [DT.Text]
deSerialise = map canonText . DT.splitOn fs


escChar :: Char
escChar = '\\'


fs :: DT.Text
fs = DT.singleton ':'


mappings, revMappings :: [(DT.Text, DT.Text)]
-- mappings:  escape separator character, then double-up escape character.
-- it is deliberate that the separator character is replaced.
mappings = [ ( fs, DT.pack (escChar : [';'])), (DT.singleton escChar, DT.pack (escChar : [escChar])) ]

revMappings = map swap mappings


applyTextRepl :: (DT.Text, DT.Text) -> DT.Text -> DT.Text
applyTextRepl = uncurry DT.replace


escapeText, canonText :: DT.Text -> DT.Text

-- perform substitutions on one text item
escapeText orig = foldr applyTextRepl orig mappings

-- reverse-perform substitutions
canonText orig = foldl' (flip applyTextRepl) orig revMappings
