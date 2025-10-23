{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import System.Directory (doesFileExist)

modelFile :: FilePath
modelFile = "model.txt"

data Model = Model
  { entries :: [String],
    quit :: Bool
  }
  deriving (Show)

data Msg
  = Command String
  | Nope
  | Quit
  deriving (Show)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Command s -> model {entries = model.entries ++ [s]}
    Quit -> model {quit = True}
    Nope -> model

render :: Model -> String
render model = renderEntries model ++ debugModel model

debugModel :: Model -> String
debugModel model = "\n\n" ++ show model ++ "\n"

renderEntries :: Model -> String
renderEntries model =
  let pairs = zip [1 :: Int ..] model.entries
      entryList = map showEntry pairs
      showEntry (i, e) = show i ++ ". " ++ e
   in "Entries:\n" ++ unlines entryList

main :: IO ()
main = do
  exists <- doesFileExist modelFile
  if exists
    then do
      content <- readFile modelFile
      loop Model {entries = lines content, quit = False}
    else do
      loop Model {entries = [], quit = False}

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
lineToMsg line = case line of
  "" -> Nope
  "q" -> Quit
  _ -> Command line
