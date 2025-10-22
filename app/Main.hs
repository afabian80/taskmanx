{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

data Model = Model
  { content :: String
  }
  deriving (Show)

data Msg
  = Inc
  | Nope
  deriving (Show)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Inc -> model {content = model.content ++ "."}
    Nope -> model

render :: Model -> String
render model = show model

main :: IO ()
main = do
  let initialModel = Model {content = ""}
  loop initialModel

loop :: Model -> IO ()
loop model = do
  putStrLn $ render model
  putStrLn "Enter 'i' to add dots."
  line <- getLine
  let msg = lineToMsg line
  let newModel = update msg model
  loop newModel

lineToMsg :: String -> Msg
lineToMsg line = case line of
  "i" -> Inc
  _ -> Nope
