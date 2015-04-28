{-|
Description: Very thin wrapper of Data.Text;  easily torn
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined

As this wrapper is so thin, we might just do away with it and commit to Data.Text
-}

module TextItem (TextItem,
	Data.Text.length,
	toByteS,
	fromByteS,
	nullText,
	Data.Text.singleton,
	Data.Text.pack,
	Data.Text.unpack,
	Data.Text.concat) where


import qualified Data.Text (Text, length, empty, unpack, pack, concat, singleton)
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import qualified Data.ByteString (ByteString)


type TextItem = Data.Text.Text


toByteS :: TextItem -> Data.ByteString.ByteString
fromByteS :: Data.ByteString.ByteString -> TextItem


toByteS = TE.encodeUtf8
fromByteS = TE.decodeUtf8


nullText :: TextItem
nullText = Data.Text.empty
