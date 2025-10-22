{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

data Model = Model
  { value :: Int
  }
  deriving (Show)

data Msg
  = Inc
  | Dec
  | Nope
  deriving (Show)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Inc -> model {value = model.value + 1}
    Dec -> model {value = model.value - 1}
    Nope -> model

render :: Model -> String
render model = show model

main :: IO ()
main = do
  let initialModel = Model {value = 0}
  loop initialModel

loop :: Model -> IO ()
loop model = do
  putStrLn $ render model
  putStrLn "Enter 'i' for increment, 'd' for decrement."
  line <- getLine
  let msg = lineToMsg line
  let newModel = update msg model
  loop newModel

lineToMsg :: String -> Msg
lineToMsg line = case line of
  "i" -> Inc
  "d" -> Dec
  _ -> Nope
