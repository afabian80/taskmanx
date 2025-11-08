{-# LANGUAGE OverloadedRecordDot #-}

module View (render) where

import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import Model
import Text.Printf
import Time

colorize :: (Word8, Word8) -> String -> String
colorize (bg, fg) = decorate [48, 5, bg, 38, 5, fg]

decorate :: [Word8] -> String -> String
decorate [] text = text
decorate codes text =
    starter ++ text ++ stopper
  where
    starter = "\ESC[" ++ cs ++ "m"
    cs = intercalate ";" (map show codes)
    stopper = "\ESC[0m"

stroked :: String -> String
stroked text = "\ESC[9m" ++ text ++ "\ESC[29m"

render :: Model -> String
render model = renderCheckpointTime model ++ renderTasks model ++ renderDebugInfo model

renderCheckpointTime :: Model -> String
renderCheckpointTime model = "\t\tCheckpoint: " ++ convertPosixToTimeStr model.checkpoint model.time ++ toggleReadyInfo ++ "\n"
  where
    toggleReadyInfo = if model.hideReady then "    " ++ colorize (111, 232) "Showing only active tasks!" else ""

renderDebugInfo :: Model -> String
renderDebugInfo _ = ""

renderTasks :: Model -> String
renderTasks model =
    let taskLines :: [String]
        taskLines = map (renderTaskLine model.time model.checkpoint) filteredTasks

        modelError Nothing = ""
        modelError (Just e) = decorate [48, 5, 160, 38, 5, 231] ("ERROR: " ++ e)

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
    newTaskMarker
        ++ case t.state of
            Todo -> renderDecoratedTaskLine []
            Done -> stroked $ renderDecoratedTaskLine [38, 5, 246]
            Cancelled -> stroked $ renderDecoratedTaskLine [38, 5, 246]
            Suspended -> stroked $ renderDecoratedTaskLine [38, 5, 246]
            Failed -> stroked $ renderDecoratedTaskLine [38, 5, 196]
            Doing -> renderDecoratedTaskLine [48, 5, 214]
            Building -> renderDecoratedTaskLine [48, 5, 81]
            Waiting -> renderDecoratedTaskLine [48, 5, 146]
            Next -> renderDecoratedTaskLine [48, 5, 236, 38, 5, 231]
  where
    renderDecoratedTaskLine :: [Word8] -> String
    renderDecoratedTaskLine codes = decorate codes line
    line = printf "%2d.%s%s (%s) @%s %s" t.taskID newMarker t.title ageData t.topic deadlineInfo
    deadlineInfo = renderDeadlineInfo t.deadline modelTime t.state
    newMarker =
        if modelTime - t.timestamp < 120
            then " ■ "
            else " "
    newTaskMarker =
        if t.timestamp > checkpointTime
            then " " ++ decorate [38, 5, 112] "■" ++ " "
            else "   "
    ageData = renderTime modelTime t.timestamp

renderDeadlineInfo :: Maybe Integer -> Integer -> TaskState -> String
renderDeadlineInfo maybeDeadline modelTime taskState =
    if taskState `elem` [Done, Cancelled, Suspended, Failed]
        then
            ""
        else case maybeDeadline of
            Nothing -> ""
            Just deadlineTime ->
                if deadlineTime <= modelTime
                    then decorate [48, 5, 196, 38, 5, 231] "TIMED OUT!"
                    else deadlineInfo deadlineTime
  where
    deadlineInfo endTS = decorate [48, 5, 112, 38, 5, 232] ("[by " ++ convertPosixToTimeStr endTS modelTime ++ "]")
