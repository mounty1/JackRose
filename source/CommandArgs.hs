{-|
Description: Build command-line-parameters object from raw @[String]@
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}

{-# LANGUAGE OverloadedStrings #-}

module CommandArgs (args, CmdLineArgs(..)) where


import qualified System.Environment as SE (getArgs)


data CmdLineArgs = CmdLineArgs {
		configName :: Maybe String,
		debuggery :: Bool
	}


args :: IO CmdLineArgs
args = SE.getArgs >>= argsToMap


argsToMap :: [String] -> IO CmdLineArgs
argsToMap list = return $ populate (CmdLineArgs Nothing False) list


populate :: CmdLineArgs -> [String] -> CmdLineArgs
populate argsMap [] = argsMap
populate argsMap (('-': 'c' : more) : argument : rest) = populate' argsMap' (('-' : more) : rest) where
	argsMap' = argsMap{configName = Just argument}
populate argsMap (('-': 'd' : more) : rest) = populate' argsMap' (('-' : more) : rest) where
	argsMap' = argsMap{debuggery = True}
populate _ (other : _) = error $ other ++ ": out of place in arguments"

populate' :: CmdLineArgs -> [String] -> CmdLineArgs
populate' argsMap ("-" : rest) = populate argsMap rest
populate' argsMap other = populate argsMap other
