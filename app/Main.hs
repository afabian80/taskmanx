{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

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
    ts :: Integer
  }
  deriving (Show, Read, Eq)

data Model = Model
  { tasks :: [Task],
    quit :: Bool,
    _error :: Maybe String,
    timestamp :: Integer
  }
  deriving (Show)

data Msg
  = Nope
  | Quit
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
    Command line -> handleLine line model

handleLine :: InputLine -> Model -> Model
handleLine line model =
  case command of
    Right (NewTask task) ->
      model
        { tasks = model.tasks ++ [Task task Todo model.timestamp],
          _error = Nothing
        }
    Right (DeleteTask task) -> deleteTask model task
    Right (SetTaskState newState indexText) -> setTaskStateByIndexText model indexText newState
    Left e -> model {_error = Just e}
  where
    command = commandFromInput line

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
-- renderDebugInfo model = "\n\n" ++ show model ++ "\n"
renderDebugInfo _ = ""

renderTasks :: Model -> String
renderTasks model =
  let indexTaskPairs :: [(Int, Task)]
      indexTaskPairs = zip [1 :: Int ..] model.tasks

      taskLines :: [String]
      taskLines = map (renderIndexedTask model.timestamp) indexTaskPairs

      modelError Nothing = ""
      modelError (Just e) = "ERROR: " ++ e
   in "\nTasks:\n======\n" ++ unlines taskLines ++ "\n" ++ modelError model._error

renderIndexedTask :: Integer -> (Int, Task) -> String
renderIndexedTask modelTime (i, t) =
  show i
    ++ ". "
    ++ (colorize (stateColor t.state) $ renderTaskState t.state)
    ++ " "
    ++ t.title
    ++ " ("
    ++ renderTime modelTime t.ts
    ++ ")"

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
      let loadedTasks = map (\line -> read line :: Task) contentLines
      loop
        Model
          { tasks = loadedTasks,
            quit = False,
            _error = Nothing,
            timestamp = currentSeconds
          }
    else do
      loop
        Model
          { tasks = [],
            quit = False,
            _error = Nothing,
            timestamp = currentSeconds
          }

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
  if newModel.quit == True
    then do
      writeFile modelFile (unlines (map show newModel.tasks))
      putStrLn "Bye!"
    else do
      writeFile modelFile (unlines (map show newModel.tasks))
      loop newModel

getCurrentSeconds :: IO Integer
getCurrentSeconds = do
  currentTime <- getCurrentTime
  return $ floor $ nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds currentTime

inputLineToMsg :: InputLine -> Msg
inputLineToMsg (InputLine line)
  | line == "" = Nope
  | line == "q" = Quit
  | otherwise = Command (InputLine line)
