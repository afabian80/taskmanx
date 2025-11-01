{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Controller
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Time (getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Model
import System.Console.ANSI
  ( clearScreen,
    setCursorPosition,
  )
import System.Console.Haskeline
  ( Completion,
    CompletionFunc,
    InputT,
    Settings (..),
    completeWord,
    getInputLine,
    runInputT,
    simpleCompletion,
  )
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import View (render)

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

modelFile :: FilePath
modelFile = "model.txt"

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
