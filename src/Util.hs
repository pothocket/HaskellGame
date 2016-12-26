module Util where

import Data.Time
import Data.Time.Clock.POSIX

getTimeMS :: IO Int
getTimeMS = (round . (*1000)) <$> getPOSIXTime
