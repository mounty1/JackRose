{-|
Description: User has pressed a button and the form is POSTed;  work out which button.
Copyright: (c) Michael Mounteney, 2016
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module DespatchButtons (despatch) where


import qualified Yesod as Y
import qualified Yesod.Auth as YA
import qualified Foundation
import qualified Data.Text as DT (Text)
import Data.Maybe (listToMaybe, catMaybes, fromMaybe)


-- | user has pressed a button; look up associated action in table, or supply default
despatch :: a -> [(DT.Text, DT.Text -> a)] -> Foundation.Handler a
despatch deflRoute routeList = YA.requireAuthId >>= (const $ Y.runInputPost $ fmap (justOne deflRoute) $ sequenceA $ map blurb routeList)


blurb :: (DT.Text, DT.Text -> a) -> Y.FormInput Foundation.Handler (Maybe a)
blurb (label, route) = (fmap route) `fmap` (Y.iopt Y.textField label)


justOne :: a -> [Maybe a] -> a
justOne defl = fromMaybe defl . listToMaybe . catMaybes
