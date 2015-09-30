{-|
Description: mnemonic renaming of conversions Text <-> ByteString
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module TextItem (toByteS, fromByteS) where


import qualified Data.Text (Text)
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import qualified Data.ByteString (ByteString)


toByteS :: Data.Text.Text -> Data.ByteString.ByteString
fromByteS :: Data.ByteString.ByteString -> Data.Text.Text


toByteS = TE.encodeUtf8
fromByteS = TE.decodeUtf8
