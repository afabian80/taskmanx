{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

data Model = Model
  { content :: String,
    quit :: Bool
  }
  deriving (Show)

data Msg
  = Inc
  | Quit
  | Nope
  deriving (Show)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Inc -> model {content = model.content ++ "."}
    Quit -> model {quit = True}
    Nope -> model

render :: Model -> String
render model = show model

main :: IO ()
main = do
  let initialModel = Model {content = "", quit = False}
  loop initialModel

loop :: Model -> IO ()
loop model = do
  putStrLn $ render model
  putStrLn "'i' to add dots. 'q' to quit."
  line <- getLine
  let msg = lineToMsg line
  let newModel = update msg model
  if newModel.quit == True
    then
      putStrLn "Bye!"
    else
      loop newModel

lineToMsg :: String -> Msg
lineToMsg line = case line of
  "i" -> Inc
  "q" -> Quit
  _ -> Nope
