{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.List (isPrefixOf)
import System.Directory (doesFileExist)

modelFile :: FilePath
modelFile = "model.txt"

data Model = Model
  { entries :: [String],
    quit :: Bool,
    _error :: Maybe String
  }
  deriving (Show)

data Msg
  = UnknownCommand String
  | New String
  | Delete String
  | Nope
  | Quit
  deriving (Show)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    UnknownCommand s -> model {_error = Just $ "Unknown command: " ++ s}
    Quit -> model {quit = True}
    Nope -> model {_error = Nothing}
    New s -> model {entries = model.entries ++ [s], _error = Nothing}
    Delete s -> model {entries = newEntries, _error = err}
      where
        (newEntries, err) = tryToDelete model.entries s

tryToDelete :: [String] -> String -> ([String], Maybe String)
tryToDelete list item =
  if item `elem` list
    then (filter (/= item) list, Nothing)
    else (list, Just (item ++ " is not in the list!"))

render :: Model -> String
render model = renderEntries model ++ debugModel model

debugModel :: Model -> String
debugModel model = "\n\n" ++ show model ++ "\n"

renderEntries :: Model -> String
renderEntries model =
  let pairs = zip [1 :: Int ..] model.entries
      entryList = map showEntry pairs
      showEntry (i, e) = show i ++ ". " ++ e
      modelError Nothing = ""
      modelError (Just e) = "ERROR: " ++ e
   in "\nEntries:\n" ++ unlines entryList ++ "\n" ++ modelError model._error

main :: IO ()
main = do
  exists <- doesFileExist modelFile
  if exists
    then do
      content <- readFile modelFile
      loop Model {entries = lines content, quit = False, _error = Nothing}
    else do
      loop Model {entries = [], quit = False, _error = Nothing}

loop :: Model -> IO ()
loop model = do
  putStrLn $ render model
  putStrLn "Enter a command. 'q' to quit."
  line <- getLine
  let msg = lineToMsg line
  let newModel = update msg model
  if newModel.quit == True
    then do
      writeFile modelFile (unlines newModel.entries)
      putStrLn "Bye!"
    else
      loop newModel

lineToMsg :: String -> Msg
lineToMsg line
  | line == "" = Nope
  | line == "q" = Quit
  | isCommand newCommandPattern line = New $ mkCommand newCommandPattern line
  | isCommand deleteCommandPattern line = Delete $ mkCommand deleteCommandPattern line
  | otherwise = UnknownCommand line

type CommandPattern = String

newCommandPattern :: CommandPattern
newCommandPattern = "new "

deleteCommandPattern :: CommandPattern
deleteCommandPattern = "del "

mkCommand :: CommandPattern -> String -> String
mkCommand cmdPattern line = drop (length cmdPattern) line

isCommand :: CommandPattern -> String -> Bool
isCommand cmdPattern line = isPrefixOf cmdPattern line
