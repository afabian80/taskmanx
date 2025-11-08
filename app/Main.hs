{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Controller
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Model
import System.Console.ANSI (
    clearScreen,
    setCursorPosition,
 )
import System.Console.Haskeline (
    Settings (..),
    getInputLine,
    runInputT,
 )
import System.Console.Haskeline.Completion
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import View (render)

modelFile :: FilePath
modelFile = "model.txt"

main :: IO ()
main = do
    exists <- doesFileExist modelFile
    currentSeconds <- getCurrentSeconds
    if exists
        then do
            content <- readFile modelFile
            let contentLines = lines content
            let checkpointLines = concat $ take 1 contentLines
            let maybeCheckpointInteger = loadMaybeCheckpoint checkpointLines
            let loadedTasks = map (\line -> read line :: Task) (drop 1 contentLines)
            case maybeCheckpointInteger of
                Nothing -> putStrLn "Error in checkpoint line!"
                Just cp ->
                    loop
                        Model
                            { tasks = loadedTasks
                            , quit = False
                            , _error = Nothing
                            , time = currentSeconds
                            , checkpoint = cp
                            , doNotBackup = False
                            , trash = []
                            , hideReady = False
                            }
        else do
            loop
                Model
                    { tasks = []
                    , quit = False
                    , _error = Nothing
                    , time = currentSeconds
                    , checkpoint = currentSeconds
                    , doNotBackup = False
                    , trash = []
                    , hideReady = False
                    }

loadMaybeCheckpoint :: String -> Maybe Integer
loadMaybeCheckpoint s = readMaybe s :: Maybe Integer

loop :: Model -> IO ()
loop model = do
    setCursorPosition 0 0
    clearScreen
    putStrLn $ render model
    let myCompletionFunc :: CompletionFunc IO
        myCompletionFunc = completeWord Nothing " " $ \prefix -> do
            let commands = allCommands
            let matchingStrings = filter (prefix `isPrefixOf`) commands
                matchingCompletions = map simpleCompletion matchingStrings
            return matchingCompletions

    let customSettings =
            Settings
                { complete = myCompletionFunc
                , historyFile = Just "history.txt"
                , autoAddHistory = True
                }

    putStrLn "Enter a command ('q' to quit): "
    mLine <- runInputT customSettings (getInputLine ">>> ")
    case mLine of
        Nothing -> putStrLn "\nBye!"
        Just line ->
            do
                let cleanLineBegin = dropWhile isSpace line
                let cleanLineAll = reverse $ dropWhile isSpace $ reverse cleanLineBegin
                let msg = inputLineToMsg (InputLine cleanLineAll)
                currentSeconds <- getCurrentSeconds
                let newModel = update msg model{time = currentSeconds}
                let newCounter = currentSeconds `mod` 100
                let backupName = "/tmp/model." ++ show newCounter ++ ".txt"
                let modelString = unlines (show model.checkpoint : map show newModel.tasks)
                writeFile modelFile modelString
                if newModel.doNotBackup
                    then return ()
                    else
                        writeFile backupName modelString
                if not $ null newModel.trash
                    then
                        appendFile "trash.txt" (unlines newModel.trash)
                    else
                        return ()
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
    | line `elem` quitCommands = Quit
    | line `elem` checkpointCommands = Checkpoint
    | line `elem` cleanCommands = Clean
    | line `elem` toggleReadyCommands = ToggleReady
    | otherwise = Command (InputLine line)
