module Main (main) where

type Model = Int

data Msg = Update deriving (Show)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Update -> model + 1

render :: Model -> String
render model = show model

main :: IO ()
main = do
  let initialModel = 0
  loop initialModel

loop :: Model -> IO ()
loop model = do
  putStrLn $ render model
  putStrLn "Press Enter"
  _ <- getLine
  let newModel = update Update model
  loop newModel
