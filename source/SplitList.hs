{-|
Description: Generic list split;  [first n items, next n, next n, .... the rest]
Copyright: (c) Michael Mounteney, 2017
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module SplitList where


split :: Int -> [a] -> [[a]]

-- | Break a long list into sub-lists of the specified length.
split _ [] = []

split n ell = first : split n rest where
	(first, rest) = splitAt n ell
