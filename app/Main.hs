{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

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
  = Nope
  | Quit
  | Command String
  deriving (Show)

data Command = NewTask String deriving (Show)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Quit -> model {quit = True}
    Nope -> model {_error = Nothing}
    Command line -> handleLine line model

handleLine :: String -> Model -> Model
handleLine line model =
  case command of
    Right (NewTask task) -> model {entries = model.entries ++ [task]}
    Left e -> model {_error = Just e}
  where
    command = mkCommand line

mkCommand :: String -> Either String Command
mkCommand line =
  case words line of
    [] -> Left "Empty command"
    [w] -> Left ("Cannot process single word: " ++ w)
    (w : ws) ->
      if w `elem` ["new", "add"]
        then
          Right (NewTask (unwords ws))
        else
          Left ("Unknown command: " ++ w)

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
  | otherwise = Command line
