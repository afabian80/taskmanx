{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import qualified Data.Map as Map
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

newtype InputLine = InputLine String deriving (Show)

modelFile :: FilePath
modelFile = "model.txt"

data TaskState = Todo | Doing | Done deriving (Show, Read)

data Task = Task
  { title :: String,
    state :: TaskState
  }
  deriving (Show, Read)

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
    Left e -> model {_error = Just e}
  where
    command = mkCommand line

deleteTask :: Model -> String -> Model
deleteTask model task =
  case maybeIndex of
    Nothing -> deleteTaskByName model task
    Just index -> deleteTaskByIndex model index
  where
    maybeIndex = readMaybe task :: Maybe Int

deleteTaskByIndex :: Model -> Int -> Model
deleteTaskByIndex model index =
  case taskAtIndex of
    Nothing -> model {_error = Just $ "No task with index " ++ show index}
    Just task -> deleteTaskByName model task.title
  where
    pairs = zip [1 ..] model.tasks
    taskMap = Map.fromList pairs
    taskAtIndex = Map.lookup index taskMap

-- thePair = filter (\(i,t) -> if i == index then True else False) pairs
deleteTaskByName :: Model -> String -> Model
deleteTaskByName model task =
  if task `elem` taskTitles
    then
      model {tasks = newEntries}
    else
      model {_error = Just $ "Cannot delete " ++ task}
  where
    newEntries = filter (\t -> t.title /= task) model.tasks
    taskTitles = map title model.tasks

addCommands :: [String]
addCommands = ["new", "add"]

delCommands :: [String]
delCommands = ["del", "delete", "rm", "remove"]

allCommands :: [String]
allCommands = concat [addCommands, delCommands]

mkCommand :: InputLine -> Either String Command
mkCommand (InputLine line) =
  case words line of
    [] -> Left "Empty command"
    [command]
      | command `elem` allCommands -> Left $ "Not enough arguments for " ++ command
      | otherwise -> Left ("Unknown command: " ++ command)
    (command : args)
      | command `elem` addCommands -> mkNewCommand args
      | command `elem` delCommands -> mkDelCommand args
      | otherwise -> Left ("Unknown command: " ++ command)

mkNewCommand :: [String] -> Either String Command
mkNewCommand args =
  if null args
    then
      Left "Not enough arguments!"
    else
      Right (NewTask (unwords args))

mkDelCommand :: [String] -> Either String Command
mkDelCommand args =
  if null args
    then
      Left "Not enough arguments!"
    else
      Right (DeleteTask (unwords args))

render :: Model -> String
render model = renderEntries model ++ debugModel model

debugModel :: Model -> String
debugModel model = "\n\n" ++ show model ++ "\n"

renderEntries :: Model -> String
renderEntries model =
  let pairs :: [(Int, Task)]
      pairs = zip [1 :: Int ..] model.tasks

      entryList :: [String]
      entryList = map showTask pairs

      showTask :: (Int, Task) -> String
      showTask (i, t) = show i ++ ". " ++ show t.state ++ " " ++ t.title

      modelError Nothing = ""
      modelError (Just e) = "ERROR: " ++ e
   in "\nEntries:\n" ++ unlines entryList ++ "\n" ++ modelError model._error

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
  putStrLn "Enter a command. 'q' to quit."
  line <- getLine
  let msg = lineToMsg (InputLine line)
  let newModel = update msg model
  if newModel.quit == True
    then do
      writeFile modelFile (unlines (map show newModel.tasks))
      putStrLn "Bye!"
    else
      loop newModel

lineToMsg :: InputLine -> Msg
lineToMsg (InputLine line)
  | line == "" = Nope
  | line == "q" = Quit
  | otherwise = Command (InputLine line)
