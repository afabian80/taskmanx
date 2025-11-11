{-# LANGUAGE StrictData #-}

module Time (renderTime, convertPosixToTimeStr, getEpochForTodayTime) where

import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (parseTimeM)
import Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)

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

-- Converts an Integer representing POSIX seconds into a UTCTime.
integerToUTCTime :: Integer -> UTCTime
integerToUTCTime seconds = posixSecondsToUTCTime (fromIntegral seconds :: POSIXTime)

-- Converts a "HH:MM" string to a TimeOfDay structure.
-- Returns Nothing on parsing failure.
parseTimeOfDay :: String -> Maybe TimeOfDay
parseTimeOfDay = parseTimeM False defaultTimeLocale "%R"

{- | Pure function to calculate the new POSIX time in seconds.
It takes the current time (in Integer seconds), and the target time string ("HH:MM").
The resulting POSIX time will be for the current day at the specified time, in UTC.

Arguments:
1. currentEpochSeconds: The current POSIX epoch (e.g., from an API call or another IO step).
2. timeStr: The target time string (e.g., "13:00").

Returns:
A 'Maybe Integer' which contains the new epoch time if parsing succeeded,
or Nothing if the time string was invalid.
-}
getEpochForTodayTime :: Integer -> String -> Maybe Integer
getEpochForTodayTime currentEpochSeconds timeStr = do
    -- 1. Convert the input epoch back to a UTCTime.
    let nowUtc = integerToUTCTime currentEpochSeconds

    -- 2. Extract the current Day from the UTCTime.
    let today = utctDay nowUtc

    -- 3. Parse the target time string ("HH:MM") into a TimeOfDay structure.
    timeOfDay <- parseTimeOfDay timeStr

    -- 4. Create the target UTCTime by combining the current Day with the target TimeOfDay.
    -- The DiffTime is calculated from TimeOfDay
    let
        targetDiffTime = timeOfDayToTime timeOfDay -- FIXED: Using timeOfDayToTime
        targetUtc = UTCTime today targetDiffTime

    -- 5. Convert the new UTCTime back to POSIX seconds and round it to an Integer.
    let
        newEpochTime = utcTimeToPOSIXSeconds targetUtc
        result = round newEpochTime :: Integer

    return result
