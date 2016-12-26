module TimeTest where

import Data.Time
import Data.Time.Clock.POSIX

main :: IO ()
main = getTime >>= print

getTime :: IO Int
getTime = round <$> getPOSIXTime