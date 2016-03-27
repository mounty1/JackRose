{-|
Description: time stamping of scheduled items.  Seconds count since start-of-epoch.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


module TimeStamp (Stamp(..), now, timeZero, isTimeZero, plus) where


import qualified Data.DateTime as DT

newtype Stamp = Stamp { at :: Int }

-- | when time began, as far as JackRose is concerned.
epoch :: Int
epoch = DT.toSeconds $ DT.fromGregorian' 2015 1 1

-- | for stamping a new item
now :: IO Stamp
now = DT.getCurrentTime >>= DT.toSeconds >>= return . reBase

-- | internally, to convert between external and internal forms
reBase :: Int -> Stamp
reBase datetime = if datetime > epoch then Stamp $ datetime - epoch else error "Clock before epoch"

timeZero :: Stamp
timeZero = Stamp 0

isTimeZero :: Stamp -> Bool
isTimeZero (Stamp t) = t == 0

plus :: Int -> Stamp -> Stamp
plus delta (Stamp time) = if delta > 0 then Stamp $ delta + time else error "Time going backwards"
