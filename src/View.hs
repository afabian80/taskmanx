{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module View (render) where

import Data.Bits (shiftL, shiftR, xor)
import Data.Char (ord)
import Data.List (intercalate, isPrefixOf, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Word (Word8)
import Model
import System.Console.ANSI (
    ConsoleLayer (Background, Foreground),
    SGR (Reset, SetPaletteColor),
    hyperlinkCode,
    setSGRCode,
 )
import Text.Printf (printf)
import Text.Regex (mkRegex, subRegex)
import Time

errorColor :: (Word8, Word8)
errorColor = colorList !! 11

prioColor :: (Word8, Word8)
prioColor = colorList !! 7

tagColor :: (Word8, Word8)
tagColor = colorList !! 15

ipColor :: (Word8, Word8)
ipColor = colorList !! 12

urlColor :: (Word8, Word8)
urlColor = colorList !! 13

timeoutColor :: (Word8, Word8)
timeoutColor = colorList !! 10

deadlineColor :: (Word8, Word8)
deadlineColor = colorList !! 3

colorize :: (Word8, Word8) -> String -> String
colorize (bgIndex, fgIndex) text =
    setSGRCode [SetPaletteColor Background bgIndex]
        ++ setSGRCode [SetPaletteColor Foreground fgIndex]
        ++ text
        ++ setSGRCode [Reset]

stateColor :: TaskState -> (Word8, Word8)
stateColor st = case st of
    Todo -> colorList !! 9
    Doing -> colorList !! 1
    Done -> colorList !! 13
    Cancelled -> colorList !! 17
    Suspended -> colorList !! 6
    Waiting -> colorList !! 5
    Building -> colorList !! 15
    Next -> colorList !! 19
    Failed -> colorList !! 11

render :: Model -> String
render model = renderCheckpointTime model ++ renderTasks model ++ renderDebugInfo model

renderCheckpointTime :: Model -> String
renderCheckpointTime model = "\t\tCheckpoint: " ++ convertPosixToTimeStr model.checkpoint model.time ++ toggleReadyInfo ++ "\n"
  where
    toggleReadyInfo = if model.hideReady then "    " ++ colorize (111, 232) "Showing only active tasks!" else ""

renderDebugInfo :: Model -> String
renderDebugInfo _ = ""

-- where
--   colorLetters = concat $ map (\(i, c) -> colorize c (printf "%3i" i)) $ zip [0 :: Int ..] colorList

renderTasks :: Model -> String
renderTasks model =
    let taskLines :: [String]
        taskLines = map (renderTaskLine model.time model.checkpoint) filteredTasks

        modelError Nothing = ""
        modelError (Just e) = colorize errorColor ("ERROR: " ++ e)

        sortedTasks :: [Task]
        sortedTasks = sortBy (comparing topic <> comparing timestamp) model.tasks

        filteredTasks :: [Task]
        filteredTasks = filter (filterReady model.hideReady) sortedTasks
     in "Tasks:\n======\n" ++ unlines taskLines ++ "\n" ++ modelError model._error

filterReady :: Bool -> Task -> Bool
filterReady p t =
    not (p && (t.state `elem` [Done, Cancelled, Suspended, Todo, Failed]))

renderTaskLine :: Integer -> Integer -> Task -> String
renderTaskLine modelTime checkpointTime t =
    colorize (255, 196) (renderCheckpointInfo t.timestamp checkpointTime)
        ++ colorize (stateColor t.state) (renderTaskState t.state)
        ++ " "
        ++ colorize (stringToColor t.topic) (printf "%4s" t.topic)
        ++ " "
        ++ colorize (stateColor t.state) (printf "%2d." t.taskID)
        ++ newMarker
        ++ (colorizePrio . colorizeTags . colorizeIP . fixLink) t.title
        ++ " ("
        ++ colorizedAgeData
        ++ ") "
        ++ renderDeadlineInfo t.deadline modelTime t.state
  where
    ageData = renderTime modelTime t.timestamp
    colorizedAgeData =
        if modelTime - t.timestamp < 120
            then colorize ipColor ageData
            else ageData
    newMarker =
        if modelTime - t.timestamp < 120
            then colorize ipColor " "
            else " "

colorizePrio :: String -> String
colorizePrio t = newTitle
  where
    newTitle = unwords $ map colorPrio (words t)
    colorPrio w = if "/" `isPrefixOf` w then colorize prioColor w else w

colorizeTags :: String -> String
colorizeTags t = newTitle
  where
    newTitle = unwords $ map colorTag (words t)
    colorTag w = if "#" `isPrefixOf` w then colorize tagColor w else w

colorizeIP :: String -> String
colorizeIP s =
    subRegex regex s replaced
  where
    ipPattern = "([0-9]{1,3}\\.){3}[0-9]{1,3}"
    regex = mkRegex ipPattern
    replaced = colorize ipColor "\\0"

fixLink :: String -> String
fixLink s = unwords $ map shortenLink (words s)

shortenLink :: String -> String
shortenLink text =
    if "http" `isPrefixOf` text
        then
            colorize urlColor "URL:" ++ " " ++ hyperlinkCode text (intercalate "/" (reverse (take 3 (reverse (splitOn "/" text)))))
        else
            text

renderDeadlineInfo :: Maybe Integer -> Integer -> TaskState -> String
renderDeadlineInfo maybeDeadline modelTime taskState =
    if taskState `elem` [Done, Cancelled, Suspended, Failed]
        then
            ""
        else case maybeDeadline of
            Nothing -> ""
            Just deadlineTime ->
                if deadlineTime <= modelTime
                    then colorize timeoutColor "TIMED OUT!"
                    else deadlineInfo deadlineTime
  where
    deadlineInfo endTS = colorize deadlineColor ("[by " ++ convertPosixToTimeStr endTS modelTime ++ "]")

renderCheckpointInfo :: Integer -> Integer -> String
renderCheckpointInfo taskTime checkpointTime =
    if taskTime > checkpointTime then "▶️" else "  "

-- A better hash: mix bits using XOR and shifts
stringHash :: String -> Int
stringHash = foldl mix 0
  where
    mix h c =
        let x = ord c
         in (h `xor` (x + h `shiftL` 5 + h `shiftR` 2))

stringToColor :: String -> (Word8, Word8)
stringToColor s =
    let n = stringHash s
        range = length colorList
        mapped = (abs (n + 5) `mod` range)
     in colorList !! mapped

bgColorListLight :: [Word8]
bgColorListLight =
    [204, 214, 227, 190, 122, 111, 140, 213, 250, 231]

bgColorListDark :: [Word8]
bgColorListDark =
    [167, 202, 214, 106, 37, 33, 98, 170, 240, 235]

colorList :: [(Word8, Word8)]
colorList = map (,16) bgColorListLight ++ map (,231) bgColorListDark
