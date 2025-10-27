{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.Console.ANSI
import System.Directory (doesFileExist)
import Text.Printf
import Text.Read (readMaybe)

newtype InputLine = InputLine String deriving (Show)

modelFile :: FilePath
modelFile = "model.txt"

data TaskState
  = Todo
  | Doing
  | Done
  | Cancelled
  | Suspended
  | Waiting
  | Building
  | Next
  deriving (Show, Read, Eq)

renderTaskState :: TaskState -> String
renderTaskState st =
  case st of
    Todo -> "TODO  "
    Doing -> "DOING "
    Done -> "DONE  "
    Cancelled -> "CANC  "
    Suspended -> "SUSP  "
    Waiting -> "WAIT  "
    Building -> "BUILD "
    Next -> "NEXT  "

colorize :: (Color, Color) -> String -> String
colorize (bgColor, fgColor) text =
  setSGRCode [SetColor Background Vivid bgColor]
    ++ setSGRCode [SetColor Foreground Dull fgColor]
    ++ text
    ++ setSGRCode [Reset]

-- underline :: String -> String
-- underline text =
--   setSGRCode [SetUnderlining SingleUnderline]
--     ++ text
--     ++ setSGRCode [SetUnderlining NoUnderline]

data Task = Task
  { title :: String,
    state :: TaskState,
    timestamp :: Integer,
    deadline :: Maybe Integer
  }
  deriving (Show, Read, Eq)

data Model = Model
  { tasks :: [Task],
    quit :: Bool,
    _error :: Maybe String,
    time :: Integer,
    checkpoint :: Integer,
    doNotBackup :: Bool
  }
  deriving (Show)

data Msg
  = Nope
  | Quit
  | Checkpoint
  | Clean
  | Command InputLine
  deriving (Show)

data Command
  = NewTask String
  | DeleteTask String
  | SetTaskState TaskState String
  | Deadline String
  deriving (Show)

update :: Msg -> Model -> Model
update msg tempModel =
  case msg of
    Quit -> model {quit = True}
    Nope -> model {doNotBackup = True}
    Checkpoint -> model {checkpoint = model.time}
    Clean -> model {tasks = cleanUpTasks model}
    Command line -> handleLine line model
  where
    model = tempModel {_error = Nothing, doNotBackup = False}

cleanUpTasks :: Model -> [Task]
cleanUpTasks model = newTasks
  where
    newTasks = filter (toBeCleaned model.checkpoint) model.tasks
    toBeCleaned :: Integer -> Task -> Bool
    toBeCleaned checkpointTime task =
      not (task.state `elem` [Done, Cancelled] && task.timestamp <= checkpointTime)

handleLine :: InputLine -> Model -> Model
handleLine line model =
  case command of
    Right (NewTask taskTitle) ->
      model
        { tasks = model.tasks ++ [newTask]
        }
      where
        newTask =
          Task
            { title = newTitle,
              state = newState,
              timestamp = model.time,
              deadline = newDeadline
            }
        newTitle = unwords $ filter (not . isPrefixOf "@") (words taskTitle)
        -- hyperTitle = unwords $ map fixLink (words newTitle)
        newDeadline = calculateDeadline taskTitle model.time
        newState = if "/job/" `isInfixOf` newTitle then Building else Todo
    Right (DeleteTask task) -> deleteTask model task
    Right (SetTaskState newState indexText) -> setTaskStateByIndexText model indexText newState
    Right (Deadline args) -> updateDeadline model args
    Left e -> model {_error = Just e}
  where
    command = commandFromInput line

updateDeadline :: Model -> String -> Model
updateDeadline model args =
  model {tasks = newTasks}
  where
    newTasks = tryUpdateDeadline model maybeIndex maybeNewDeadline
    maybeIndex = parseTaskIndex (words args)
    maybeNewDeadline = parseNewDeadline (drop 1 $ words args)

    parseTaskIndex :: [String] -> Maybe Integer
    parseTaskIndex [] = Nothing
    parseTaskIndex (x : _) = readMaybe x :: Maybe Integer

    parseNewDeadline :: [String] -> Maybe String
    parseNewDeadline [] = Nothing
    parseNewDeadline (x : _) = Just x

    -- this is ugly, need to learn more :)
    tryUpdateDeadline :: Model -> Maybe Integer -> Maybe String -> [Task]
    tryUpdateDeadline m mi ms =
      case mi of
        Nothing -> m.tasks
        Just i -> case lookupTaskAtIndex m.tasks (fromIntegral i) of
          Nothing -> m.tasks
          Just t ->
            map
              ( \other ->
                  if t.title == other.title
                    then case ms of
                      Nothing -> other
                      Just s -> case (readMaybe s :: Maybe Integer) of
                        Nothing -> other
                        Just newNumber ->
                          other {deadline = Just (model.time + newNumber * 60)}
                    else other
              )
              m.tasks

commandFromInput :: InputLine -> Either String Command
commandFromInput (InputLine line) =
  case words line of
    [] -> Left "Empty command"
    [command]
      | command `elem` allCommands -> Left $ "Not enough arguments for " ++ command
      | otherwise -> Left ("Unknown command: " ++ command)
    (command : args)
      | command `elem` addCommands -> makeSafeCommand NewTask args
      | command `elem` delCommands -> makeSafeCommand DeleteTask args
      | command `elem` todoCommands -> makeSafeCommand (SetTaskState Todo) args
      | command `elem` doingCommands -> makeSafeCommand (SetTaskState Doing) args
      | command `elem` doneCommands -> makeSafeCommand (SetTaskState Done) args
      | command `elem` cancelCommands -> makeSafeCommand (SetTaskState Cancelled) args
      | command `elem` suspendCommands -> makeSafeCommand (SetTaskState Suspended) args
      | command `elem` deadlineCommands -> makeSafeCommand Deadline args
      | command `elem` waitCommands -> makeSafeCommand (SetTaskState Waiting) args
      | command `elem` buildCommands -> makeSafeCommand (SetTaskState Building) args
      | command `elem` nextCommands -> makeSafeCommand (SetTaskState Next) args
      | otherwise -> Left ("Unknown command: " ++ command)

makeSafeCommand :: (String -> Command) -> [String] -> Either String Command
makeSafeCommand taskConstructor args =
  if null args
    then
      Left "Not enough arguments!"
    else
      Right (taskConstructor (unwords args))

convertPosixToTimeStr :: Integer -> String
convertPosixToTimeStr ts =
  timeStr
  where
    timeStr = formatTime defaultTimeLocale "%a %H:%M" posixTime
    posixTime = posixSecondsToUTCTime (realToFrac (ts + zoneDiff) :: POSIXTime)
    zoneDiff = 3600

-- If the text contains "@number"" exactly once then return the number as minutes
-- added to the second parameter in seconds
calculateDeadline :: String -> Integer -> Maybe Integer
calculateDeadline taskTitle startTimeSeconds = endTime
  where
    endTime = if duration == 0 then Nothing else Just (startTimeSeconds + 60 * duration)

    atWords = filter (isPrefixOf "@") (words taskTitle)

    getDurationText [d] = Just d
    getDurationText _ = Nothing

    maybeDurationText = getDurationText atWords

    durationText Nothing = "0"
    durationText (Just text) = drop 1 text

    durationInt = readMaybe (durationText maybeDurationText) :: Maybe Integer

    getDurationInt Nothing = 0
    getDurationInt (Just i) = i

    duration = getDurationInt durationInt

setTaskStateByIndexText :: Model -> String -> TaskState -> Model
setTaskStateByIndexText model indexText newState =
  case maybeIndex of
    Nothing -> model {_error = Just "This command needs an index argument!"}
    Just index -> setTaskStateByIndexInt model index newState
  where
    maybeIndex = readMaybe indexText :: Maybe Int

setTaskStateByIndexInt :: Model -> Int -> TaskState -> Model
setTaskStateByIndexInt model index newState =
  case taskAtIndex of
    Nothing -> model {_error = Just $ "No task with index " ++ show index}
    Just task ->
      model
        { tasks = map (updateTaskState task newState model.time) model.tasks
        }
  where
    taskAtIndex = lookupTaskAtIndex model.tasks index
    updateTaskState taskToMatch st newTS otherTask =
      if otherTask == taskToMatch
        then otherTask {state = st, timestamp = newTS}
        else otherTask

deleteTask :: Model -> String -> Model
deleteTask model task =
  case maybeIndex of
    Nothing -> deleteTaskByTitle model task
    Just index -> deleteTaskByIndex model index
  where
    maybeIndex = readMaybe task :: Maybe Int

deleteTaskByIndex :: Model -> Int -> Model
deleteTaskByIndex model index =
  case taskAtIndex of
    Nothing -> model {_error = Just $ "No task with index " ++ show index}
    Just task -> deleteTaskByTitle model task.title
  where
    taskAtIndex = lookupTaskAtIndex model.tasks index

lookupTaskAtIndex :: [Task] -> Int -> Maybe Task
lookupTaskAtIndex taskList index =
  Map.lookup index taskMap
  where
    pairs = zip [1 ..] taskList
    taskMap = Map.fromList pairs

deleteTaskByTitle :: Model -> String -> Model
deleteTaskByTitle model taskTitle =
  if taskTitle `elem` taskTitles
    then
      model {tasks = newTaskList}
    else
      model {_error = Just $ "Cannot delete " ++ taskTitle}
  where
    newTaskList = filter (\t -> t.title /= taskTitle) model.tasks
    taskTitles = map title model.tasks

addCommands :: [String]
addCommands = ["new", "add", "a"]

delCommands :: [String]
delCommands = ["del", "delete", "d", "rm", "remove"]

doneCommands :: [String]
doneCommands = ["done"]

todoCommands :: [String]
todoCommands = ["todo"]

doingCommands :: [String]
doingCommands = ["doing", "now"]

cancelCommands :: [String]
cancelCommands = ["cancel", "canc"]

suspendCommands :: [String]
suspendCommands = ["suspend"]

deadlineCommands :: [String]
deadlineCommands = ["deadline"]

waitCommands :: [String]
waitCommands = ["wait"]

buildCommands :: [String]
buildCommands = ["build", "building", "b"]

nextCommands :: [String]
nextCommands = ["next"]

allCommands :: [String]
allCommands =
  concat
    [ addCommands,
      delCommands,
      doneCommands,
      todoCommands,
      doingCommands,
      cancelCommands,
      suspendCommands,
      deadlineCommands,
      waitCommands,
      buildCommands,
      nextCommands
    ]

render :: Model -> String
render model = renderCheckpointTime model ++ renderTasks model ++ renderDebugInfo model

renderCheckpointTime :: Model -> String
renderCheckpointTime model = "\t\tCheckpoint: " ++ convertPosixToTimeStr model.checkpoint ++ "\n"

renderDebugInfo :: Model -> String
renderDebugInfo _ = ""

renderTasks :: Model -> String
renderTasks model =
  let indexTaskPairs :: [(Int, Task)]
      indexTaskPairs = zip [1 :: Int ..] model.tasks

      taskLines :: [String]
      taskLines = map (renderIndexedTask model.time model.checkpoint) indexTaskPairs

      modelError Nothing = ""
      modelError (Just e) = colorize (Red, White) ("ERROR: " ++ e)
   in "Tasks:\n======\n" ++ unlines taskLines ++ "\n" ++ modelError model._error

renderIndexedTask :: Integer -> Integer -> (Int, Task) -> String
renderIndexedTask modelTime checkpointTime (i, t) =
  printf "%3d" i
    ++ ". "
    ++ colorize (stateColor t.state) (renderTaskState t.state)
    ++ " "
    ++ renderCheckpointInfo t.timestamp checkpointTime
    ++ pimp t.title
    ++ " ("
    ++ renderTime modelTime t.timestamp
    ++ ") "
    ++ renderDeadlineInfo t.deadline modelTime t.state

pimp :: String -> String
pimp s = unwords $ map shortenLink (words s)

shortenLink :: String -> String
shortenLink text =
  if "http" `isPrefixOf` text
    then
      hyperlinkCode text (intercalate "/" (reverse (take 3 (reverse (splitOn "/" text)))))
    else
      text

renderDeadlineInfo :: Maybe Integer -> Integer -> TaskState -> String
renderDeadlineInfo maybeDeadline modelTime taskState =
  if taskState `elem` [Done, Cancelled, Suspended]
    then
      ""
    else case maybeDeadline of
      Nothing -> ""
      Just deadlineTime ->
        if deadlineTime <= modelTime
          then colorize (Red, White) "TIMED OUT!"
          else deadlineInfo deadlineTime
  where
    deadlineInfo endTS = colorize (Cyan, Black) ("[by " ++ convertPosixToTimeStr endTS ++ "]")

renderCheckpointInfo :: Integer -> Integer -> [Char]
renderCheckpointInfo taskTime checkpointTime =
  if taskTime > checkpointTime then "🟡 " else ""

stateColor :: TaskState -> (Color, Color)
stateColor st = case st of
  Todo -> (White, Black)
  Doing -> (Yellow, Black)
  Done -> (Green, Black)
  Cancelled -> (Red, White)
  Suspended -> (Red, White)
  Waiting -> (Cyan, Black)
  Building -> (Cyan, Black)
  Next -> (Black, White)

secondsInDay :: Integer
secondsInDay = 86400

secondsInHour :: Integer
secondsInHour = 3600

secondsInMinute :: Integer
secondsInMinute = 60

toDHMS :: Integer -> (Integer, Integer, Integer, Integer)
toDHMS totalSeconds =
  let days = totalSeconds `div` secondsInDay
      remainingDay = totalSeconds `mod` secondsInDay

      hours = remainingDay `div` secondsInHour
      remainingHour = remainingDay `mod` secondsInHour

      minutes = remainingHour `div` secondsInMinute

      seconds = remainingHour `mod` secondsInMinute
   in (days, hours, minutes, seconds)

renderTime :: Integer -> Integer -> String
renderTime modelTime taskTime = showTimeRounded (d, h, m, s)
  where
    (d, h, m, s) = toDHMS (modelTime - taskTime)

-- showTime :: (Integer, Integer, Integer, Integer) -> String
-- showTime (0, 0, 0, s) = show s ++ "s"
-- showTime (0, 0, m, s) = show m ++ "m " ++ show s ++ "s"
-- showTime (0, h, m, s) = show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"
-- showTime (d, h, m, s) = show d ++ "d " ++ show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"

showTimeRounded :: (Integer, Integer, Integer, Integer) -> String
showTimeRounded (0, 0, 0, s) = show s ++ "s"
showTimeRounded (0, 0, m, _) = show m ++ "m"
showTimeRounded (0, h, _, _) = show h ++ "h"
showTimeRounded (d, _, _, _) = show d ++ "d"

main :: IO ()
main = do
  exists <- doesFileExist modelFile
  currentSeconds <- getCurrentSeconds
  if exists
    then do
      content <- readFile modelFile
      let contentLines = lines content
      let checkpointLines = take 1 contentLines
      let maybeCheckpointInteger = loadMaybeCheckpoint checkpointLines
      let loadedTasks = map (\line -> read line :: Task) (drop 1 contentLines)
      case maybeCheckpointInteger of
        Nothing -> putStrLn "Error in checkpoint line!"
        Just cp ->
          loop
            Model
              { tasks = loadedTasks,
                quit = False,
                _error = Nothing,
                time = currentSeconds,
                checkpoint = cp,
                doNotBackup = False
              }
    else do
      loop
        Model
          { tasks = [],
            quit = False,
            _error = Nothing,
            time = currentSeconds,
            checkpoint = currentSeconds,
            doNotBackup = False
          }

loadMaybeCheckpoint :: [String] -> Maybe Integer
loadMaybeCheckpoint [s] = readMaybe s :: Maybe Integer
loadMaybeCheckpoint _ = Nothing

loop :: Model -> IO ()
loop model = do
  setCursorPosition 0 0
  clearScreen
  putStrLn $ render model
  putStrLn "Enter a command ('q' to quit): "
  line <- getLine
  let msg = inputLineToMsg (InputLine line)
  currentSeconds <- getCurrentSeconds
  let newModel =
        update
          msg
          model
            { time = currentSeconds
            }
  let newCounter = currentSeconds `mod` 100
  let backupName = "/tmp/model." ++ show newCounter ++ ".txt"
  let modelString = unlines (show model.checkpoint : map show newModel.tasks)
  writeFile modelFile modelString
  if newModel.doNotBackup then mempty else writeFile backupName modelString
  if newModel.quit
    then do
      putStrLn "Bye!"
    else do
      loop newModel

getCurrentSeconds :: IO Integer
getCurrentSeconds = do floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

inputLineToMsg :: InputLine -> Msg
inputLineToMsg (InputLine line)
  | line == "" = Nope
  | line == "q" = Quit
  | line == "quit" = Quit
  | line == "checkpoint" = Checkpoint
  | line == "cp" = Checkpoint
  | line == "clean" = Clean
  | otherwise = Command (InputLine line)
