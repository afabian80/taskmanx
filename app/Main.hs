{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (doesFileExist)
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
  deriving (Show, Read, Eq)

renderTaskState :: TaskState -> String
renderTaskState st =
  case st of
    Todo -> "TODO  "
    Doing -> "DOING "
    Done -> "DONE  "
    Cancelled -> "CANC  "
    Suspended -> "SUSP  "

data Color = ColorYellow | ColorGreen | ColorRed | ColorWhite | ColorReset

instance Show Color where
  show ColorYellow = "\ESC[43;30m"
  show ColorWhite = "\ESC[47;30m"
  show ColorGreen = "\ESC[42;30m"
  show ColorRed = "\ESC[41;37m"
  show ColorReset = "\ESC[0m"

colorize :: Color -> String -> String
colorize color text = show color ++ text ++ show ColorReset

data Task = Task
  { title :: String,
    state :: TaskState,
    ts :: Integer,
    deadlineMinutes :: Maybe Integer
  }
  deriving (Show, Read, Eq)

data Model = Model
  { tasks :: [Task],
    quit :: Bool,
    _error :: Maybe String,
    timestamp :: Integer,
    checkpoint :: Integer
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
  deriving (Show)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Quit -> model {quit = True}
    Nope -> model {_error = Nothing}
    Checkpoint -> model {checkpoint = model.timestamp, _error = Nothing}
    Clean -> model {tasks = cleanUpTasks model, _error = Nothing}
    Command line -> handleLine line model

cleanUpTasks :: Model -> [Task]
cleanUpTasks model = newTasks
  where
    newTasks = filter (toBeCleaned model.checkpoint) model.tasks
    toBeCleaned :: Integer -> Task -> Bool
    toBeCleaned checkpointTime task =
      not (task.state `elem` [Done, Cancelled] && task.ts <= checkpointTime)

handleLine :: InputLine -> Model -> Model
handleLine line model =
  case command of
    Right (NewTask taskTitle) ->
      model
        { tasks = model.tasks ++ [newTask],
          _error = Nothing
        }
      where
        newTask =
          Task
            { title = taskTitle,
              state = Todo,
              ts = model.timestamp,
              deadlineMinutes = calculateDeadline taskTitle model.timestamp
            }
    Right (DeleteTask task) -> deleteTask model task
    Right (SetTaskState newState indexText) -> setTaskStateByIndexText model indexText newState
    Left e -> model {_error = Just e}
  where
    command = commandFromInput line

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
        { tasks = map (updateTaskState task newState model.timestamp) model.tasks,
          _error = Nothing
        }
  where
    taskAtIndex = lookupTaskAtIndex model.tasks index
    updateTaskState taskToMatch st newTS otherTask =
      if otherTask == taskToMatch
        then otherTask {state = st, ts = newTS}
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
      model {tasks = newTaskList, _error = Nothing}
    else
      model {_error = Just $ "Cannot delete " ++ taskTitle}
  where
    newTaskList = filter (\t -> t.title /= taskTitle) model.tasks
    taskTitles = map title model.tasks

addCommands :: [String]
addCommands = ["new", "add"]

delCommands :: [String]
delCommands = ["del", "delete", "rm", "remove"]

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

allCommands :: [String]
allCommands =
  concat
    [ addCommands,
      delCommands,
      doneCommands,
      todoCommands,
      doingCommands,
      cancelCommands,
      suspendCommands
    ]

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
      | otherwise -> Left ("Unknown command: " ++ command)

makeSafeCommand :: (String -> Command) -> [String] -> Either String Command
makeSafeCommand taskConstructor args =
  if null args
    then
      Left "Not enough arguments!"
    else
      Right (taskConstructor (unwords args))

render :: Model -> String
render model = renderTasks model ++ renderDebugInfo model

renderDebugInfo :: Model -> String
renderDebugInfo _ = ""

renderTasks :: Model -> String
renderTasks model =
  let indexTaskPairs :: [(Int, Task)]
      indexTaskPairs = zip [1 :: Int ..] model.tasks

      taskLines :: [String]
      taskLines = map (renderIndexedTask model.timestamp model.checkpoint) indexTaskPairs

      modelError Nothing = ""
      modelError (Just e) = "ERROR: " ++ e
   in "\nTasks:\n======\n" ++ unlines taskLines ++ "\n" ++ modelError model._error

renderIndexedTask :: Integer -> Integer -> (Int, Task) -> String
renderIndexedTask modelTime checkpointTime (i, t) =
  show i
    ++ ". "
    ++ colorize (stateColor t.state) (renderTaskState t.state)
    ++ " "
    ++ renderCheckpointInfo t.ts checkpointTime
    ++ t.title
    ++ " ("
    ++ renderTime modelTime t.ts
    ++ ") "
    ++ renderDeadlineInfo t.deadlineMinutes modelTime t.state

renderDeadlineInfo :: Maybe Integer -> Integer -> TaskState -> String
renderDeadlineInfo maybeDeadline modelTime taskState =
  if taskState `elem` [Done, Cancelled, Suspended]
    then
      ""
    else case maybeDeadline of
      Nothing -> ""
      Just deadlineTime ->
        if deadlineTime <= modelTime
          then colorize ColorRed "TIMED OUT!"
          else ""

renderCheckpointInfo :: Integer -> Integer -> [Char]
renderCheckpointInfo taskTime checkpointTime =
  if taskTime > checkpointTime then "🟡 " else ""

stateColor :: TaskState -> Color
stateColor st = case st of
  Todo -> ColorWhite
  Doing -> ColorYellow
  Done -> ColorGreen
  Cancelled -> ColorRed
  Suspended -> ColorRed

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
renderTime modelTime taskTime = showTime (d, h, m, s)
  where
    (d, h, m, s) = toDHMS (modelTime - taskTime)

showTime :: (Integer, Integer, Integer, Integer) -> String
showTime (0, 0, 0, s) = show s ++ "s"
showTime (0, 0, m, s) = show m ++ "m " ++ show s ++ "s"
showTime (0, h, m, s) = show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"
showTime (d, h, m, s) = show d ++ "d " ++ show h ++ "h " ++ show m ++ "m " ++ show s ++ "s"

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
                timestamp = currentSeconds,
                checkpoint = cp
              }
    else do
      loop
        Model
          { tasks = [],
            quit = False,
            _error = Nothing,
            timestamp = currentSeconds,
            checkpoint = currentSeconds
          }

loadMaybeCheckpoint :: [String] -> Maybe Integer
loadMaybeCheckpoint [s] = readMaybe s :: Maybe Integer
loadMaybeCheckpoint _ = Nothing

loop :: Model -> IO ()
loop model = do
  putStrLn $ render model
  putStrLn "Enter a command ('q' to quit): "
  line <- getLine
  let msg = inputLineToMsg (InputLine line)
  currentSeconds <- getCurrentSeconds
  let newModel =
        update
          msg
          model
            { timestamp = currentSeconds
            }
  let newCounter = currentSeconds `mod` 100
  let backupName = "/tmp/model." ++ show newCounter ++ ".txt"
  let modelString = unlines (show model.checkpoint : map show newModel.tasks)
  writeFile modelFile modelString
  writeFile backupName modelString
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
  | line == "checkpoint" = Checkpoint
  | line == "clean" = Clean
  | otherwise = Command (InputLine line)
