module Util where

import Data.Time
import Data.Time.Clock.POSIX

getTimeMS :: IO Int
getTimeMS = (round . (*1000)) <$> getPOSIXTime

clamp :: (Ord a) => a -> a -> a -> a
clamp min max x | x < min   = min
                | x > max   = max
                | otherwise = x
