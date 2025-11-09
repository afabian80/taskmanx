{-# LANGUAGE StrictData #-}

module Time (renderTime, convertPosixToTimeStr) where

import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)

data TaskTime = TaskTime Integer Integer Integer Integer deriving (Show, Read)

toDHMS :: Integer -> TaskTime
toDHMS totalSeconds =
    let days = totalSeconds `div` secondsInDay
        remainingDay = totalSeconds `mod` secondsInDay

        hours = remainingDay `div` secondsInHour
        remainingHour = remainingDay `mod` secondsInHour

        minutes = remainingHour `div` secondsInMinute

        seconds = remainingHour `mod` secondsInMinute

        secondsInDay = 86400

        secondsInHour = 3600

        secondsInMinute = 60
     in TaskTime days hours minutes seconds

renderTime :: Integer -> Integer -> String
renderTime modelTime taskTime = showTimeRounded tt
  where
    tt = toDHMS (modelTime - taskTime)

showTimeRounded :: TaskTime -> String
showTimeRounded (TaskTime 0 0 0 s) = show s ++ "s"
showTimeRounded (TaskTime 0 0 m _) = show m ++ "m"
showTimeRounded (TaskTime 0 h _ _) = show h ++ "h"
showTimeRounded (TaskTime d _ _ _) = show d ++ "d"

convertPosixToTimeStr :: Integer -> Integer -> String
convertPosixToTimeStr ts modelTime =
    timeStr
  where
    timeStr =
        if ts - modelTime > 3600 * 24 * 5 -- 5 days
            then
                formatTime defaultTimeLocale "%F" posixTime
            else
                formatTime defaultTimeLocale "%a %H:%M" posixTime
    posixTime = posixSecondsToUTCTime (realToFrac (ts + zoneDiff) :: POSIXTime)
    zoneDiff = 3600
