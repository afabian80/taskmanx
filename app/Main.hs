{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Map as Map
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

data Task = Task
  { title :: String,
    state :: TaskState
  }
  deriving (Show, Read, Eq)

data Model = Model
  { tasks :: [Task],
    quit :: Bool,
    _error :: Maybe String
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
    Right (NewTask task) -> model {tasks = model.tasks ++ [Task task Todo]}
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
    Just task -> model {tasks = map (updateTaskState task newState) model.tasks}
  where
    taskAtIndex = lookupTaskAtIndex model.tasks index
    updateTaskState taskToMatch st otherTask =
      if otherTask == taskToMatch
        then otherTask {state = st}
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
suspendCommands = ["suspend", "susp"]

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
renderDebugInfo model = "\n\n" ++ show model ++ "\n"

renderTasks :: Model -> String
renderTasks model =
  let indexTaskPairs :: [(Int, Task)]
      indexTaskPairs = zip [1 :: Int ..] model.tasks

      taskLines :: [String]
      taskLines = map renderIndexedTask indexTaskPairs

      renderIndexedTask :: (Int, Task) -> String
      renderIndexedTask (i, t) = show i ++ ". " ++ show t.state ++ " " ++ t.title

      modelError Nothing = ""
      modelError (Just e) = "ERROR: " ++ e
   in "\nEntries:\n" ++ unlines taskLines ++ "\n" ++ modelError model._error

main :: IO ()
main = do
  exists <- doesFileExist modelFile
  if exists
    then do
      content <- readFile modelFile
      let contentLines = lines content
      let loadedTasks = map (\line -> read line :: Task) contentLines
      loop Model {tasks = loadedTasks, quit = False, _error = Nothing}
    else do
      loop Model {tasks = [], quit = False, _error = Nothing}

loop :: Model -> IO ()
loop model = do
  putStrLn $ render model
  putStrLn "Enter a command ('q' to quit): "
  line <- getLine
  let msg = inputLineToMsg (InputLine line)
  let newModel = update msg model
  if newModel.quit == True
    then do
      writeFile modelFile (unlines (map show newModel.tasks))
      putStrLn "Bye!"
    else
      loop newModel

inputLineToMsg :: InputLine -> Msg
inputLineToMsg (InputLine line)
  | line == "" = Nope
  | line == "q" = Quit
  | otherwise = Command (InputLine line)
