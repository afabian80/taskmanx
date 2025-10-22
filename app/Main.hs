{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

modelFile :: FilePath
modelFile = "model.txt"

data Model = Model
  { entries :: [String],
    quit :: Bool
  }
  deriving (Show)

data Msg
  = Add String
  | Inc
  | Quit
  | Nope
  deriving (Show)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Add s -> model {entries = model.entries ++ [s]}
    Inc -> model {entries = model.entries ++ ["."]}
    Quit -> model {quit = True}
    Nope -> model

render :: Model -> String
render model = show model

main :: IO ()
main = do
  content <- readFile modelFile
  let initialModel = Model {entries = lines content, quit = False}
  loop initialModel

loop :: Model -> IO ()
loop model = do
  putStrLn $ render model
  putStrLn "'i' to add dots. 'q' to quit."
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
  "i" -> Inc
  "q" -> Quit
  _ -> Add line
