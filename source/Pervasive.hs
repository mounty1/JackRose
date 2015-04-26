module Pervasive (TextItem,
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
