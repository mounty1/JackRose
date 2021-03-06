{-|
Description: Build command-line-parameters object from raw @[String]@
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module CommandArgs (args, CmdLineArgs(..)) where


import qualified System.Environment as SE (getArgs)


-- | The data that can be set via the command line.
data CmdLineArgs = CmdLineArgs {
		configName :: Maybe String,	-- ^ default defined in module @SiteConfigFromFile@.
		debuggery :: Bool	-- ^ default @False@
	}


-- | Get the raw command line arguments and populate a @CmdLineArgs@ instance
-- with the data specified therein.
args :: IO CmdLineArgs
args = populate (CmdLineArgs Nothing False) `fmap` SE.getArgs


-- Take the head of what's left of the command line and change the @CmdLineArgs@
-- accordingly, then tail on to the rest of the arguments.
populate :: CmdLineArgs -> [String] -> CmdLineArgs
populate argsMap [] = argsMap
populate argsMap (('-': 'c' : more) : argument : rest) = populate' argsMap' (('-' : more) : rest) where
	argsMap' = argsMap{configName = Just argument}
populate argsMap (('-': 'd' : more) : rest) = populate' argsMap' (('-' : more) : rest) where
	argsMap' = argsMap{debuggery = True}
populate _ (other : _) = error $ other ++ ": out of place in arguments"


-- Sub-function for @populate@ that allows an empty "-" for when
-- all options letters have been consumed.
populate' :: CmdLineArgs -> [String] -> CmdLineArgs
populate' argsMap ("-" : rest) = populate argsMap rest
populate' argsMap other = populate argsMap other
