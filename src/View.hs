{-# LANGUAGE OverloadedRecordDot #-}

module View (render) where

import Data.List (intercalate, isPrefixOf, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Word (Word8)
import Model
import System.Console.ANSI
  ( ConsoleLayer (Background, Foreground),
    SGR (Reset, SetPaletteColor),
    hyperlinkCode,
    setSGRCode,
  )
import Text.Printf (printf)
import Text.Regex (mkRegex, subRegex)
import Time

errorColor :: (Word8, Word8)
errorColor = (160, 255)

prioColor :: (Word8, Word8)
prioColor = (171, 234)

tagColor :: (Word8, Word8)
tagColor = (33, 234)

ipColor :: (Word8, Word8)
ipColor = (226, 234)

urlColor :: (Word8, Word8)
urlColor = (32, 255)

timeoutColor :: (Word8, Word8)
timeoutColor = (196, 255)

deadlineColor :: (Word8, Word8)
deadlineColor = (44, 232)

colorize :: (Word8, Word8) -> String -> String
colorize (bgIndex, fgIndex) text =
  setSGRCode [SetPaletteColor Background bgIndex]
    ++ setSGRCode [SetPaletteColor Foreground fgIndex]
    ++ text
    ++ setSGRCode [Reset]

stateColor :: TaskState -> (Word8, Word8)
stateColor st = case st of
  Todo -> (255, 232)
  Doing -> (228, 232)
  Done -> (76, 232)
  Cancelled -> (161, 255)
  Suspended -> (214, 232)
  Waiting -> (44, 232)
  Building -> (32, 255)
  Next -> (236, 255)
  Failed -> (196, 255)

render :: Model -> String
render model = renderCheckpointTime model ++ renderTasks model ++ renderDebugInfo model

renderCheckpointTime :: Model -> String
renderCheckpointTime model = "\t\tCheckpoint: " ++ convertPosixToTimeStr model.checkpoint model.time ++ "\n"

renderDebugInfo :: Model -> String
renderDebugInfo _ = ""

renderTasks :: Model -> String
renderTasks model =
  let taskLines :: [String]
      taskLines = map (renderTaskLine model.time model.checkpoint) sortedTasks

      modelError Nothing = ""
      modelError (Just e) = colorize errorColor ("ERROR: " ++ e)

      sortedTasks :: [Task]
      sortedTasks = sortBy (comparing topic <> comparing timestamp) model.tasks
   in "Tasks:\n======\n" ++ unlines taskLines ++ "\n" ++ modelError model._error

renderTaskLine :: Integer -> Integer -> Task -> String
renderTaskLine modelTime checkpointTime t =
  " "
    ++ colorize (stateColor t.state) (renderTaskState t.state)
    ++ renderCheckpointInfo t.timestamp checkpointTime
    ++ colorize (stateColor t.state) (printf "%4s" t.topic)
    ++ " "
    ++ colorize (stateColor t.state) (printf "%2d." t.taskID)
    ++ " "
    ++ (colorizePrio . colorizeTags . colorizeIP . fixLink) t.title
    ++ " ("
    ++ renderTime modelTime t.timestamp
    ++ ") "
    ++ renderDeadlineInfo t.deadline modelTime t.state

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

renderCheckpointInfo :: Integer -> Integer -> [Char]
renderCheckpointInfo taskTime checkpointTime =
  if taskTime > checkpointTime then "█" else " "