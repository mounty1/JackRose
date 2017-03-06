{-|
Description: Extract integer value from Text, if possible.
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module MaybeIntValue (maybeIntValue) where


import qualified Data.Text as DT (Text)
import qualified Data.Text.Read as DTR (decimal)


-- | Represents an optional number in a configuration file.
maybeIntValue :: Integral i => DT.Text -> Maybe i
maybeIntValue = reduceIt . DTR.decimal


reduceIt :: Integral i => Either String (i, DT.Text) -> Maybe i
reduceIt (Right (n, "")) = Just n
reduceIt _ = Nothing
