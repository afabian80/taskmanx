{-# LANGUAGE OverloadedRecordDot #-}

module Model
  ( Model (..),
    Task (..),
    TaskState (..),
    InputLine (..),
    renderTaskState,
    Msg (..),
    Command (..),
  )
where

import Text.Printf

data Model = Model
  { tasks :: [Task],
    quit :: Bool,
    _error :: Maybe String,
    time :: Integer,
    checkpoint :: Integer,
    doNotBackup :: Bool,
    trash :: [String],
    hideReady :: Bool
  }
  deriving (Show)

data Task = Task
  { title :: String,
    state :: TaskState,
    timestamp :: Integer,
    deadline :: Maybe Integer,
    taskID :: Integer,
    topic :: String
  }
  deriving (Show, Read, Eq)

data TaskState
  = Todo
  | Doing
  | Done
  | Cancelled
  | Suspended
  | Waiting
  | Building
  | Next
  | Failed
  deriving (Show, Read, Eq)

newtype InputLine = InputLine String deriving (Show)

renderTaskState :: TaskState -> String
renderTaskState st =
  case st of
    Todo -> printf "%-9s" "TODO"
    Doing -> printf "%-9s" "DOING"
    Done -> printf "%-9s" "DONE"
    Cancelled -> printf "%-9s" "CANCELLED"
    Suspended -> printf "%-9s" "SUSPENDED"
    Waiting -> printf "%-9s" "WAITING"
    Building -> printf "%-9s" "BUILDING"
    Next -> printf "%-9s" "NEXT"
    Failed -> printf "%-9s" "FAILED"

data Msg
  = Nope
  | Quit
  | Checkpoint
  | Clean
  | Command InputLine
  | ToggleReady
  deriving (Show)

data Command
  = NewTask String
  | DeleteTask String
  | SetTaskState TaskState String
  | Deadline String
  | Undeadline String
  | SetNumber String
  | SetTopic String
  deriving (Show)
