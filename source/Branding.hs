{-|
Description: Names used in presentation and for file names etc.
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE OverloadedStrings #-}


module Branding (visibleName, innerName) where


import qualified Data.Text as DT (Text, toLower)


visibleName, innerName :: DT.Text

visibleName = "JackRose"

innerName = DT.toLower visibleName
