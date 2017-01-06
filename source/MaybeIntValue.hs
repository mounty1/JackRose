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
maybeIntValue :: DT.Text -> Maybe Int
maybeIntValue = reduceIt . DTR.decimal


reduceIt :: Either String (Int, DT.Text) -> Maybe Int
reduceIt (Right (n, "")) = Just n
reduceIt _ = Nothing
