module SplitList where


split :: Int -> [a] -> [[a]]

split _ [] = []

split n ell = first : split n rest where
	(first, rest) = splitAt n ell
