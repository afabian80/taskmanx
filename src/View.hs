{-# LANGUAGE OverloadedRecordDot #-}

module View (render) where

import Data.List (intercalate, isPrefixOf, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Model
import System.Console.ANSI
  ( Color (Black, Cyan, Green, Magenta, Red, White, Yellow),
    ColorIntensity (..),
    ConsoleLayer (Background, Foreground),
    SGR (Reset, SetColor),
    hyperlinkCode,
    setSGRCode,
  )
import Text.Printf (printf)
import Text.Regex (mkRegex, subRegex)
import Time

errorColor :: ((Color, ColorIntensity), (Color, ColorIntensity))
errorColor = ((Red, Dull), (White, Dull))

prioColor :: ((Color, ColorIntensity), (Color, ColorIntensity))
prioColor = ((Magenta, Vivid), (Black, Dull))

tagColor :: ((Color, ColorIntensity), (Color, ColorIntensity))
tagColor = ((Cyan, Vivid), (Black, Dull))

ipColor :: ((Color, ColorIntensity), (Color, ColorIntensity))
ipColor = ((Yellow, Vivid), (Black, Dull))

urlColor :: ((Color, ColorIntensity), (Color, ColorIntensity))
urlColor = ((Magenta, Vivid), (White, Vivid))

timeoutColor :: ((Color, ColorIntensity), (Color, ColorIntensity))
timeoutColor = ((Red, Dull), (White, Vivid))

deadlineColor :: ((Color, ColorIntensity), (Color, ColorIntensity))
deadlineColor = ((Cyan, Vivid), (Black, Dull))

colorize :: ((Color, ColorIntensity), (Color, ColorIntensity)) -> String -> String
colorize ((bgColor, bgIntensity), (fgColor, fgIntensity)) text =
  setSGRCode [SetColor Background bgIntensity bgColor]
    ++ setSGRCode [SetColor Foreground fgIntensity fgColor]
    ++ text
    ++ setSGRCode [Reset]

stateColor :: TaskState -> ((Color, ColorIntensity), (Color, ColorIntensity))
stateColor st = case st of
  Todo -> ((White, Vivid), (Black, Dull))
  Doing -> ((Yellow, Vivid), (Black, Dull))
  Done -> ((Green, Vivid), (Black, Dull))
  Cancelled -> ((Magenta, Vivid), (White, Vivid))
  Suspended -> ((Magenta, Vivid), (White, Vivid))
  Waiting -> ((Cyan, Vivid), (Black, Dull))
  Building -> ((Cyan, Dull), (Black, Dull))
  Next -> ((Black, Dull), (White, Vivid))
  Failed -> ((Red, Dull), (White, Vivid))

render :: Model -> String
render model = renderCheckpointTime model ++ renderTasks model ++ renderDebugInfo model

renderCheckpointTime :: Model -> String
renderCheckpointTime model = "\t\tCheckpoint: " ++ convertPosixToTimeStr model.checkpoint ++ "\n"

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
    ++ colorize prioColor (printf "%4s" t.topic)
    ++ " "
    ++ printf "%2d." t.taskID
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
    deadlineInfo endTS = colorize deadlineColor ("[by " ++ convertPosixToTimeStr endTS ++ "]")

renderCheckpointInfo :: Integer -> Integer -> [Char]
renderCheckpointInfo taskTime checkpointTime =
  if taskTime > checkpointTime then "🟡" else "  "