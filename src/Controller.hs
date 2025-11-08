{-# LANGUAGE OverloadedRecordDot #-}

module Controller (update, sortedCommands, quitCommands, checkpointCommands, cleanCommands, toggleReadyCommands, parseDeadlineToSeconds, allCommands) where

import Data.Char (isNumber)
import Data.List (find, isInfixOf, isPrefixOf, sort)
import Data.Set qualified as Set
import Model
import Text.Read (readMaybe)
import Text.Regex (mkRegex, subRegex)

update :: Msg -> Model -> Model
update msg tempModel =
    case msg of
        Quit -> model{quit = True}
        Nope -> model{doNotBackup = True}
        Checkpoint -> model{checkpoint = model.time}
        Clean -> model{tasks = keep, trash = map title waste}
        Command line -> handleLine line model
        ToggleReady -> model{hideReady = not model.hideReady}
  where
    model = tempModel{_error = Nothing, doNotBackup = False, trash = []}
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
            if null deadlineWords
                then
                    model
                        { tasks =
                            model.tasks
                                ++ [ Task
                                        { title = newTitle
                                        , state = newState
                                        , timestamp = model.time
                                        , deadline = Nothing
                                        , taskID = generateTaskId model.tasks 1000
                                        , topic = if null newTopic then defaultTopic else newTopic
                                        }
                                   ]
                        }
                else case parseDeadlineToSeconds deadlineWords of
                    Left e -> model{_error = Just e}
                    Right sec ->
                        model
                            { tasks =
                                model.tasks
                                    ++ [ Task
                                            { title = newTitle
                                            , state = newState
                                            , timestamp = model.time
                                            , deadline = Just (model.time + sec)
                                            , taskID = generateTaskId model.tasks 1000
                                            , topic = if null newTopic then defaultTopic else newTopic
                                            }
                                       ]
                            }
          where
            deadlineWords = drop 1 $ concat $ filter (isPrefixOf "+") (words taskTitle)
            filterWords = ["+", "@"]
            startsWihFilterWords w = any (`isPrefixOf` w) filterWords
            newTitle = unwords $ filter (not . startsWihFilterWords) (words taskTitle)
            newState
                | "/job/" `isInfixOf` newTitle = Building
                | model.hideReady = Doing
                | otherwise = Todo
            newTopic = unwords $ map (drop 1) $ filter (isPrefixOf "@") (words taskTitle)
        Right (DeleteTask task) -> deleteTask model task
        Right (SetTaskState newState indexText) -> setTaskStateByIndexText model indexText newState
        Right (Deadline args) -> updateDeadline model args
        Right (Undeadline args) -> updateDeadline model (args ++ " 0")
        Right (SetNumber args) -> updateBuildNumberStr model args
        Right (SetTopic args) -> setTopic model args
        Left e -> model{_error = Just e}
  where
    command = commandFromInput line

setTopic :: Model -> String -> Model
setTopic model args =
    case mIndex of
        Nothing -> model{_error = Just ("Invalid number: " ++ indexStr)}
        Just index -> updateTopic model index newTopic
  where
    indexStr = concat (take 1 (words args))
    mIndex = readMaybe indexStr :: Maybe Integer
    newTopic = concat (take 1 (drop 1 (words args)))

updateTopic :: Model -> Integer -> String -> Model
updateTopic model index newTopic =
    case mTask of
        Nothing -> model{_error = Just ("No task with index " ++ show index)}
        Just task -> model{tasks = setTaskTopic task}
  where
    mTask = lookupTaskAtIndex model.tasks index

    setTaskTopic :: Task -> [Task]
    setTaskTopic task =
        map (setTopicOnMatch task) model.tasks

    setTopicOnMatch :: Task -> Task -> Task
    setTopicOnMatch theTask aTask =
        if theTask.taskID == aTask.taskID
            then
                if null newTopic
                    then
                        aTask{topic = defaultTopic}
                    else
                        aTask{topic = newTopic}
            else
                aTask

defaultTopic :: String
defaultTopic = "   "

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
        Nothing -> model{_error = Just ("Not a valid task index: " ++ indexStr)}
        Just index ->
            case mNumber of
                Nothing -> model{_error = Just ("Not a valid number: " ++ numberStr)}
                Just buildNumber -> updateBuildNumber model index buildNumber
  where
    indexStr = concat (take 1 (words args))
    numberStr = concat (take 1 (drop 1 (words args)))
    mIndex = readMaybe indexStr :: Maybe Integer
    mNumber = readMaybe numberStr :: Maybe Int

updateBuildNumber :: Model -> Integer -> Int -> Model
updateBuildNumber model index buildNumber =
    case mTask of
        Nothing -> model{_error = Just ("No task with index " ++ show index)}
        Just task -> model{tasks = map (updateTask task buildNumber) model.tasks}
  where
    mTask = lookupTaskAtIndex model.tasks index

    updateTask :: Task -> Int -> Task -> Task
    updateTask theTask bn aTask =
        if theTask.title == aTask.title
            then
                aTask{title = replaceBuildNumber bn aTask.title}
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

parseDeadlineToSeconds :: String -> Either String Integer
parseDeadlineToSeconds text =
    case mUnitMultiplier of
        Nothing -> Left ("Invalid time unit: " ++ deadlineUnitStr)
        Just multiplier -> case mDeadline of
            Nothing -> Left ("Invalid number: " ++ deadlineNumberStr)
            Just n -> Right (n * multiplier)
  where
    deadlineNumberStr = takeWhile isNumber text
    deadlineUnitStr = dropWhile isNumber text
    mDeadline = readMaybe deadlineNumberStr :: Maybe Integer

    mUnitMultiplier :: Maybe Integer
    mUnitMultiplier = case deadlineUnitStr of
        "s" -> Just 1
        "" -> Just 60
        "m" -> Just 60
        "h" -> Just (60 * 60)
        "d" -> Just (24 * 60 * 60)
        _ -> Nothing

updateDeadline :: Model -> String -> Model
updateDeadline model args =
    case mTask of
        Nothing -> model{_error = Just ("No task with index " ++ indexStr)}
        Just task -> case deadlineSeconds of
            Left e -> model{_error = Just e}
            Right sec -> updateModel model task sec
  where
    indexStr = concat $ take 1 (words args) -- the first word
    deadlineStr = concat $ take 1 $ drop 1 $ words args -- the second word
    deadlineSeconds = parseDeadlineToSeconds deadlineStr
    mIndex = readMaybe indexStr :: Maybe Integer
    mTask = case mIndex of
        Nothing -> Nothing
        Just index -> lookupTaskAtIndex model.tasks index

    updateModel :: Model -> Task -> Integer -> Model
    updateModel theModel theTask sec =
        theModel{tasks = map (updateTask theModel theTask sec) theModel.tasks}

    updateTask :: Model -> Task -> Integer -> Task -> Task
    updateTask theModel theTask sec aTask =
        if theTask.title == aTask.title
            then aTask{deadline = newDeadline, timestamp = model.time}
            else aTask
      where
        newDeadline = if sec == 0 then Nothing else Just (theModel.time + sec)

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
            | command `elem` undeadlineCommands -> makeSafeCommand Undeadline args
            | command `elem` waitCommands -> makeSafeCommand (SetTaskState Waiting) args
            | command `elem` buildCommands -> makeSafeCommand (SetTaskState Building) args
            | command `elem` nextCommands -> makeSafeCommand (SetTaskState Next) args
            | command `elem` failedCommands -> makeSafeCommand (SetTaskState Failed) args
            | command `elem` numberCommands -> makeSafeCommand SetNumber args
            | command `elem` topicCommands -> makeSafeCommand SetTopic args
            | otherwise -> Left ("Unknown command: " ++ command)

makeSafeCommand :: (String -> Command) -> [String] -> Either String Command
makeSafeCommand taskConstructor args =
    if null args
        then
            Left "Not enough arguments!"
        else
            Right (taskConstructor (unwords args))

setTaskStateByIndexText :: Model -> String -> TaskState -> Model
setTaskStateByIndexText model indexText newState =
    case maybeIndex of
        Nothing -> model{_error = Just "This command needs an index argument!"}
        Just index -> setTaskStateByIndexInt model index newState
  where
    maybeIndex = readMaybe indexText :: Maybe Integer

setTaskStateByIndexInt :: Model -> Integer -> TaskState -> Model
setTaskStateByIndexInt model index newState =
    case taskAtIndex of
        Nothing -> model{_error = Just $ "No task with index " ++ show index}
        Just task ->
            model
                { tasks = map (updateTaskState task newState model.time) model.tasks
                }
  where
    taskAtIndex = lookupTaskAtIndex model.tasks index
    updateTaskState taskToMatch st newTS otherTask =
        if otherTask == taskToMatch
            then otherTask{state = st, timestamp = newTS}
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
        Nothing -> model{_error = Just $ "No task with index " ++ show index}
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
            model{tasks = newTaskList, trash = model.trash ++ [taskTitle]}
        else
            model{_error = Just $ "Cannot delete " ++ taskTitle}
  where
    newTaskList = filter (\t -> t.title /= taskTitle) model.tasks
    taskTitles = map title model.tasks

addCommands :: [String]
addCommands = ["new", "add", "a"]

delCommands :: [String]
delCommands = ["delete", "remove", "d"]

doneCommands :: [String]
doneCommands = ["done"]

todoCommands :: [String]
todoCommands = ["todo"]

doingCommands :: [String]
doingCommands = ["doing", "now", "start"]

cancelCommands :: [String]
cancelCommands = ["cancel"]

suspendCommands :: [String]
suspendCommands = ["suspend"]

deadlineCommands :: [String]
deadlineCommands = ["deadline"]

undeadlineCommands :: [String]
undeadlineCommands = ["undeadline"]

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

topicCommands :: [String]
topicCommands = ["settopic", "topic"]

toggleReadyCommands :: [String]
toggleReadyCommands = ["toggle", "toggleReady", "t"]

allCommands :: [String]
allCommands =
    concat
        [ addCommands
        , delCommands
        , doneCommands
        , todoCommands
        , doingCommands
        , cancelCommands
        , suspendCommands
        , deadlineCommands
        , waitCommands
        , buildCommands
        , nextCommands
        , quitCommands
        , checkpointCommands
        , cleanCommands
        , failedCommands
        , numberCommands
        , topicCommands
        , undeadlineCommands
        , toggleReadyCommands
        ]

sortedCommands :: [String]
sortedCommands = sort allCommands
