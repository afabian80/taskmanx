{-# LANGUAGE OverloadedRecordDot #-}

module View (render) where

import Data.List (groupBy, intercalate, isPrefixOf, sortBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import Model
import Text.Printf
import Time

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
    toggleReadyInfo = if model.hideReady then "    " ++ decorate [48, 5, 111, 38, 5, 232] "Showing only active tasks!" else ""

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
        sortedTasks = sortBy (comparing topic) model.tasks

        filteredTasks :: [Task]
        filteredTasks = filter (filterReady model.hideReady) sortedTasks

        sameTopic :: Task -> Task -> Bool
        sameTopic t1 t2 = t1.topic == t2.topic

        groupedTasks :: [[Task]]
        groupedTasks = groupBy sameTopic filteredTasks

        renderTopic :: [Task] -> [String]
        renderTopic = map (renderTaskLine model)
     in -- I could add empty lines between different topic, but it does not look good.
        "Tasks:\n======\n" ++ taskLines ++ "\n" ++ modelError model._error

filterReady :: Bool -> Task -> Bool
filterReady p t =
    not (p && (t.state `elem` [Done, Cancelled, Suspended, Todo, Failed, Next]))

renderTaskLine :: Model -> Task -> String
renderTaskLine model task =
    newTaskMarker
        ++ case task.state of
            Todo -> renderDecoratedTaskLine todoColor
            Done -> stroked $ renderDecoratedTaskLine finishedColor
            Cancelled -> stroked $ renderDecoratedTaskLine finishedColor
            Suspended -> renderDecoratedTaskLine suspendColor
            Failed -> stroked $ renderDecoratedTaskLine failedColor
            Doing -> renderDecoratedTaskLine doingColor
            Building -> renderDecoratedTaskLine buildingColor
            Waiting -> renderDecoratedTaskLine waitingColor
            Next -> renderDecoratedTaskLine nextColor
  where
    todoColor = [48, 5, 252]
    finishedColor = [38, 5, 248]
    suspendColor = [48, 5, 252, 38, 5, 105]
    failedColor = [38, 5, 197]
    doingColor = [48, 5, 214]
    buildingColor = [48, 5, 81]
    waitingColor = [48, 5, 146]
    nextColor = [48, 5, 236, 38, 5, 231]

    renderDecoratedTaskLine :: [Word8] -> String
    renderDecoratedTaskLine codes = decorate codes line

    line :: String
    line =
        printf
            "%2d.│%s%s │%5s │%*s %s"
            task.taskID
            newMarker
            limitedTitleWithPadding
            ageData
            maxTopicLen
            task.topic
            deadlineInfo

    deadlineInfo :: String
    deadlineInfo = renderDeadlineInfo task.deadline model.time task.state

    newMarker :: String
    newMarker =
        if model.time - task.timestamp < 5
            then "■ "
            else "  "

    newTaskMarker :: String
    newTaskMarker =
        if task.timestamp > model.checkpoint
            then " " ++ decorate [38, 5, 112] "■" ++ " "
            else "   "

    ageData :: String
    ageData = printf "%s" (renderTime model.time task.timestamp)

    urlMaskedTitle :: String
    urlMaskedTitle = urlMask task.title

    urlMask :: String -> String
    urlMask text =
        if model.hideUrl
            then unwords (map replaceUrl (words text))
            else text

    limitedTitle :: String
    limitedTitle = take maxTitleLen urlMaskedTitle

    maxTitleLen :: Int
    maxTitleLen = maximum (map (length . urlMask . title) model.tasks)

    limitedTitleWithPadding :: String
    limitedTitleWithPadding = printf "%-*s" maxTitleLen limitedTitle

    replaceUrl :: String -> String
    replaceUrl w = if "http" `isPrefixOf` w then "[link]" else w

    maxTopicLen :: Int
    maxTopicLen = maximum (map (length . topic) model.tasks)

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
