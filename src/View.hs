{-# LANGUAGE OverloadedRecordDot #-}

module View (render) where

import Data.List (groupBy, intercalate, isPrefixOf, sortBy)
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
    let taskLines :: String
        taskLines =
            intercalate "\n" $
                map (intercalate "\n" . renderTopic) groupedTasks

        modelError Nothing = ""
        modelError (Just e) = decorate [48, 5, 160, 38, 5, 231] ("ERROR: " ++ e)

        sortedTasks :: [Task]
        sortedTasks = sortBy (comparing topic <> comparing timestamp) model.tasks

        filteredTasks :: [Task]
        filteredTasks = filter (filterReady model.hideReady) sortedTasks

        sameTopic :: Task -> Task -> Bool
        sameTopic t1 t2 = t1.topic == t2.topic

        groupedTasks :: [[Task]]
        groupedTasks = groupBy sameTopic filteredTasks

        renderTopic :: [Task] -> [String]
        renderTopic = map (renderTaskLine model.time model.checkpoint model.hideUrl)
     in -- I could add empty lines between different topic, but it does not look good.
        "Tasks:\n======\n" ++ taskLines ++ "\n" ++ modelError model._error

filterReady :: Bool -> Task -> Bool
filterReady p t =
    not (p && (t.state `elem` [Done, Cancelled, Suspended, Todo, Failed, Next]))

renderTaskLine :: Integer -> Integer -> Bool -> Task -> String
renderTaskLine modelTime checkpointTime hu t =
    newTaskMarker
        ++ case t.state of
            Todo -> renderDecoratedTaskLine todoColor
            Done -> stroked $ renderDecoratedTaskLine finishedColor
            Cancelled -> stroked $ renderDecoratedTaskLine finishedColor
            Suspended -> stroked $ renderDecoratedTaskLine finishedColor
            Failed -> stroked $ renderDecoratedTaskLine failedColor
            Doing -> renderDecoratedTaskLine doingColor
            Building -> renderDecoratedTaskLine buildingColor
            Waiting -> renderDecoratedTaskLine waitingColor
            Next -> renderDecoratedTaskLine nextColor
  where
    todoColor = [48, 5, 252]
    finishedColor = [38, 5, 248]
    failedColor = [38, 5, 214]
    doingColor = [48, 5, 214]
    buildingColor = [48, 5, 81]
    waitingColor = [48, 5, 146]
    nextColor = [48, 5, 236, 38, 5, 231]
    renderDecoratedTaskLine :: [Word8] -> String
    renderDecoratedTaskLine codes = decorate codes line
    line = printf "%2d.%s%s %5s %-8s %s" t.taskID newMarker limitedTitle ageData t.topic deadlineInfo
    deadlineInfo = renderDeadlineInfo t.deadline modelTime t.state
    newMarker =
        if modelTime - t.timestamp < 5
            then "■ "
            else "  "
    newTaskMarker =
        if t.timestamp > checkpointTime
            then " " ++ decorate [38, 5, 112] "■" ++ " "
            else "   "
    ageData :: String
    ageData = printf "%s" (renderTime modelTime t.timestamp)
    urlMaskedTitle =
        if hu
            then unwords (map replaceUrl (words t.title))
            else t.title
    limitedTitle :: String
    limitedTitle = printf "%-40s" (take 40 urlMaskedTitle)
    replaceUrl :: String -> String
    replaceUrl w = if "http" `isPrefixOf` w then "...URL..." else w

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
