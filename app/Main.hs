{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Char (isNumber, isSpace)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.Console.ANSI
import System.Console.Haskeline
import System.Directory (doesFileExist)
import Text.Printf
import Text.Read (readMaybe)
import Text.Regex (mkRegex, subRegex)

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

searchFunc :: String -> [Completion]
searchFunc str =
  map simpleCompletion $
    filter (str `isPrefixOf`) sortedCommands

myCompletionFunc :: (Monad m) => CompletionFunc m
myCompletionFunc =
  completeWord
    Nothing -- No escape character
    " \t" -- Space and tab are word break characters
    (return . searchFunc)

mySettings :: Settings IO
mySettings =
  Settings
    { historyFile = Just "history.txt",
      complete = myCompletionFunc,
      autoAddHistory = True
    }

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
  | Failed
  deriving (Show, Read, Eq)

renderTaskState :: TaskState -> String
renderTaskState st =
  case st of
    Todo -> printf "%-9s" "TODO"
    Doing -> printf "%-9s" "DOING"
    Done -> printf "%-9s" "DONE"
    Cancelled -> printf "%-9s" "CANCELLED"
    Suspended -> printf "%-9s" "SUSPENDED"
    Waiting -> printf "%-9s" "WAITING"
    Building -> printf "%-9s" "BUILDING"
    Next -> printf "%-9s" "NEXT"
    Failed -> printf "%-9s" "FAILED"

colorize :: ((Color, ColorIntensity), (Color, ColorIntensity)) -> String -> String
colorize ((bgColor, bgIntensity), (fgColor, fgIntensity)) text =
  setSGRCode [SetColor Background bgIntensity bgColor]
    ++ setSGRCode [SetColor Foreground fgIntensity fgColor]
    ++ text
    ++ setSGRCode [Reset]

data Task = Task
  { title :: String,
    state :: TaskState,
    timestamp :: Integer,
    deadline :: Maybe Integer,
    taskID :: Integer,
    topic :: String
  }
  deriving (Show, Read, Eq)

data Model = Model
  { tasks :: [Task],
    quit :: Bool,
    _error :: Maybe String,
    time :: Integer,
    checkpoint :: Integer,
    doNotBackup :: Bool,
    trash :: [String]
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
  | SetNumber String
  deriving (Show)

update :: Msg -> Model -> Model
update msg tempModel =
  case msg of
    Quit -> model {quit = True}
    Nope -> model {doNotBackup = True}
    Checkpoint -> model {checkpoint = model.time}
    Clean -> model {tasks = keep, trash = map title waste}
    Command line -> handleLine line model
  where
    model = tempModel {_error = Nothing, doNotBackup = False, trash = []}
    (keep, waste) = cleanUpTasks model

cleanUpTasks :: Model -> ([Task], [Task])
cleanUpTasks model = (newTasks, tasksToRemove)
  where
    newTasks = filter (toBeCleaned model.checkpoint) model.tasks
    tasksToRemove = filter (not . toBeCleaned model.checkpoint) model.tasks
    toBeCleaned :: Integer -> Task -> Bool
    toBeCleaned checkpointTime task =
      not (task.state `elem` [Done, Cancelled, Failed] && task.timestamp <= checkpointTime)

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
              deadline = newDeadline,
              taskID = generateTaskId model.tasks 1000,
              topic = if null newTopic then "   " else newTopic
            }
        filterWords = ["+", "@"]
        startsWihFilterWords w = any (\prefix -> isPrefixOf prefix w) filterWords
        newTitle = unwords $ filter (not . startsWihFilterWords) (words taskTitle)
        newDeadline = calculateDeadline taskTitle model.time
        newState = if "/job/" `isInfixOf` newTitle then Building else Todo
        newTopic = unwords $ map (drop 1) $ filter (isPrefixOf "@") (words taskTitle)
    Right (DeleteTask task) -> deleteTask model task
    Right (SetTaskState newState indexText) -> setTaskStateByIndexText model indexText newState
    Right (Deadline args) -> updateDeadline model args
    Right (SetNumber args) -> updateBuildNumberStr model args
    Left e -> model {_error = Just e}
  where
    command = commandFromInput line

generateTaskId :: [Task] -> Integer -> Integer
generateTaskId usedIds maxId =
  let usedSet = Set.fromList (map taskID usedIds)
      potentialIds = [1 ..]
      firstAvailable = find (\n -> n > maxId || not (Set.member n usedSet)) potentialIds
   in case firstAvailable of
        Just n | n <= maxId -> n
        _ -> error "too many tasks"

updateBuildNumberStr :: Model -> String -> Model
updateBuildNumberStr model args =
  case mIndex of
    Nothing -> model {_error = Just ("Not a valid task index: " ++ indexStr)}
    Just index ->
      case mNumber of
        Nothing -> model {_error = Just ("Not a valid number: " ++ numberStr)}
        Just buildNumber -> updateBuildNumber model index buildNumber
  where
    indexStr = concat (take 1 (words args))
    numberStr = concat (take 1 (drop 1 (words args)))
    mIndex = readMaybe indexStr :: Maybe Integer
    mNumber = readMaybe numberStr :: Maybe Int

updateBuildNumber :: Model -> Integer -> Int -> Model
updateBuildNumber model index buildNumber =
  case mTask of
    Nothing -> model {_error = Just ("No task with index " ++ show index)}
    Just task -> model {tasks = map (updateTask task buildNumber) model.tasks}
  where
    mTask = lookupTaskAtIndex model.tasks index

    updateTask :: Task -> Int -> Task -> Task
    updateTask theTask bn aTask =
      if theTask.title == aTask.title
        then
          aTask {title = replaceBuildNumber bn aTask.title}
        else
          aTask

    replaceBuildNumberInWord :: Int -> String -> String
    replaceBuildNumberInWord nn word =
      if "http://" `isPrefixOf` word || "https://" `isPrefixOf` word
        then subRegex regex word replaced
        else word
      where
        regexPattern = "[0-9]+/?$"
        regex = mkRegex regexPattern
        replaced = show nn ++ "/"

    replaceBuildNumber :: Int -> String -> String
    replaceBuildNumber nn t =
      unwords updatedWords
      where
        wordsList = words t
        updatedWords = map (replaceBuildNumberInWord nn) wordsList

updateDeadline :: Model -> String -> Model
updateDeadline model args =
  case mDeadline of
    Nothing -> model {_error = Just ("invalid deadline: " ++ deadlineStr)}
    Just dl -> case mTask of
      Nothing -> model {_error = Just ("no task with id " ++ indexStr)}
      Just task -> case mUnitMultiplier of
        Nothing -> model {_error = Just ("invalid unit: " ++ deadlineUnitStr)}
        Just multi -> updateModel model task dl multi
  where
    indexStr = concat $ take 1 (words args) -- the first word
    deadlineStr = concat $ take 1 $ drop 1 $ words args -- the second word
    deadlineNumberStr = takeWhile isNumber deadlineStr
    deadlineUnitStr = dropWhile isNumber deadlineStr
    mIndex = readMaybe indexStr :: Maybe Integer
    mDeadline = readMaybe deadlineNumberStr :: Maybe Integer
    mUnitMultiplier = case deadlineUnitStr of
      "" -> Just 60
      "m" -> Just 60
      "h" -> Just (60 * 60)
      "d" -> Just (24 * 60 * 60)
      _ -> Nothing
    mTask = case mIndex of
      Nothing -> Nothing
      Just index -> lookupTaskAtIndex model.tasks index

    updateModel :: Model -> Task -> Integer -> Integer -> Model
    updateModel theModel theTask d multi =
      theModel {tasks = map (updateTask theModel theTask d multi) theModel.tasks}

    updateTask :: Model -> Task -> Integer -> Integer -> Task -> Task
    updateTask theModel theTask d multi aTask =
      if theTask.title == aTask.title
        then aTask {deadline = Just (theModel.time + d * multi), timestamp = model.time}
        else aTask

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
      | command `elem` failedCommands -> makeSafeCommand (SetTaskState Failed) args
      | command `elem` numberCommands -> makeSafeCommand SetNumber args
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

-- If the text contains "+number"" exactly once then return the number as minutes
-- added to the second parameter in seconds
calculateDeadline :: String -> Integer -> Maybe Integer
calculateDeadline taskTitle startTimeSeconds = endTime
  where
    endTime = if duration == 0 then Nothing else Just (startTimeSeconds + 60 * duration)

    atWords = filter (isPrefixOf "+") (words taskTitle)

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
    maybeIndex = readMaybe indexText :: Maybe Integer

setTaskStateByIndexInt :: Model -> Integer -> TaskState -> Model
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
    maybeIndex = readMaybe task :: Maybe Integer

deleteTaskByIndex :: Model -> Integer -> Model
deleteTaskByIndex model index =
  case taskAtIndex of
    Nothing -> model {_error = Just $ "No task with index " ++ show index}
    Just task -> deleteTaskByTitle model task.title
  where
    taskAtIndex = lookupTaskAtIndex model.tasks index

lookupTaskAtIndex :: [Task] -> Integer -> Maybe Task
lookupTaskAtIndex taskList index =
  if null matches then Nothing else safeHead matches
  where
    matches = take 1 $ filter (\t -> t.taskID == index) taskList
    safeHead :: [Task] -> Maybe Task
    safeHead [] = Nothing
    safeHead (x : _) = Just x

deleteTaskByTitle :: Model -> String -> Model
deleteTaskByTitle model taskTitle =
  if taskTitle `elem` taskTitles
    then
      model {tasks = newTaskList, trash = model.trash ++ [taskTitle]}
    else
      model {_error = Just $ "Cannot delete " ++ taskTitle}
  where
    newTaskList = filter (\t -> t.title /= taskTitle) model.tasks
    taskTitles = map title model.tasks

addCommands :: [String]
addCommands = ["new", "add"]

delCommands :: [String]
delCommands = ["delete", "remove"]

doneCommands :: [String]
doneCommands = ["done"]

todoCommands :: [String]
todoCommands = ["todo"]

doingCommands :: [String]
doingCommands = ["doing", "now"]

cancelCommands :: [String]
cancelCommands = ["cancel"]

suspendCommands :: [String]
suspendCommands = ["suspend"]

deadlineCommands :: [String]
deadlineCommands = ["deadline"]

waitCommands :: [String]
waitCommands = ["wait"]

buildCommands :: [String]
buildCommands = ["building"]

nextCommands :: [String]
nextCommands = ["next"]

quitCommands :: [String]
quitCommands = ["exit", "quit", "q"]

checkpointCommands :: [String]
checkpointCommands = ["checkpoint", "cp"]

cleanCommands :: [String]
cleanCommands = ["clean"]

failedCommands :: [String]
failedCommands = ["failed"]

numberCommands :: [String]
numberCommands = ["setbuildnumber", "sbn"]

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
      nextCommands,
      quitCommands,
      checkpointCommands,
      cleanCommands,
      failedCommands,
      numberCommands
    ]

sortedCommands :: [String]
sortedCommands = sort allCommands

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
  printf "%3d" t.taskID
    ++ ". "
    ++ colorize (stateColor t.state) (renderTaskState t.state)
    ++ " "
    ++ colorize prioColor t.topic
    ++ " "
    ++ renderCheckpointInfo t.timestamp checkpointTime
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
  if taskTime > checkpointTime then "🟡 " else ""

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

showTimeRounded :: (Integer, Integer, Integer, Integer) -> String
showTimeRounded (0, 0, 0, s) = show s ++ "s"
showTimeRounded (0, 0, m, _) = show m ++ "m"
showTimeRounded (0, h, _, _) = show h ++ "h"
showTimeRounded (d, _, _, _) = show d ++ "d"

main :: IO ()
main = runInputT mySettings $ do
  exists <- liftIO $ doesFileExist modelFile
  currentSeconds <- liftIO getCurrentSeconds
  if exists
    then do
      content <- liftIO $ readFile modelFile
      let contentLines = lines content
      let checkpointLines = concat $ take 1 contentLines
      let maybeCheckpointInteger = loadMaybeCheckpoint checkpointLines
      let loadedTasks = map (\line -> read line :: Task) (drop 1 contentLines)
      case maybeCheckpointInteger of
        Nothing -> liftIO $ putStrLn "Error in checkpoint line!"
        Just cp ->
          loop
            Model
              { tasks = loadedTasks,
                quit = False,
                _error = Nothing,
                time = currentSeconds,
                checkpoint = cp,
                doNotBackup = False,
                trash = []
              }
    else do
      loop
        Model
          { tasks = [],
            quit = False,
            _error = Nothing,
            time = currentSeconds,
            checkpoint = currentSeconds,
            doNotBackup = False,
            trash = []
          }

loadMaybeCheckpoint :: String -> Maybe Integer
loadMaybeCheckpoint s = readMaybe s :: Maybe Integer

loop :: Model -> InputT IO ()
loop model = do
  liftIO $ setCursorPosition 0 0
  liftIO clearScreen
  liftIO $ putStrLn $ render model
  liftIO $ putStrLn "Enter a command ('q' to quit): "
  mLine <- getInputLine ">>> "
  case mLine of
    Nothing -> liftIO $ putStrLn "\nBye!"
    Just line -> do
      let cleanLineBegin = dropWhile isSpace line
      let cleanLineAll = reverse $ dropWhile isSpace $ reverse cleanLineBegin
      let msg = inputLineToMsg (InputLine cleanLineAll)
      currentSeconds <- liftIO getCurrentSeconds
      let newModel =
            update
              msg
              model
                { time = currentSeconds
                }
      let newCounter = currentSeconds `mod` 100
      let backupName = "/tmp/model." ++ show newCounter ++ ".txt"
      let modelString = unlines (show model.checkpoint : map show newModel.tasks)
      liftIO $ writeFile modelFile modelString
      if newModel.doNotBackup
        then return ()
        else liftIO $ writeFile backupName modelString
      if not $ null newModel.trash
        then
          liftIO $ appendFile "trash.txt" (unlines newModel.trash)
        else
          return ()
      if newModel.quit
        then do
          liftIO $ putStrLn "Bye!"
        else do
          loop newModel

getCurrentSeconds :: IO Integer
getCurrentSeconds = do floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

inputLineToMsg :: InputLine -> Msg
inputLineToMsg (InputLine line)
  | line == "" = Nope
  | line `elem` quitCommands = Quit
  | line `elem` checkpointCommands = Checkpoint
  | line `elem` cleanCommands = Clean
  | otherwise = Command (InputLine line)
