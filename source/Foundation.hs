{-# LANGUAGE OverloadedStrings #-}

module Foundation (JRState(..), siteObject) where


import qualified Pervasive (TextItem)
-- import qualified System.Environment as SE (getArgs)


data JRState = JRState {
		secureOnly :: Bool,
		sessionTimeout :: Int,
		authTable :: Pervasive.TextItem,
		keysFile :: FilePath
	}


-- this will use getArgs soon:
siteObject :: IO JRState
siteObject = return baseSiteObject


baseSiteObject :: JRState
baseSiteObject = JRState False 120 "users.sqlite" "jackrose-keys.aes"


mainConfigFileName :: Pervasive.TextItem
mainConfigFileName = "jackrose.cfg"
