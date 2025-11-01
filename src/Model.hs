{-# LANGUAGE OverloadedRecordDot #-}

module Model
  ( Model (..),
    Task (..),
    TaskState (..),
    InputLine (..),
    renderTaskState,
    Msg (..),
    Command (..),
    addCommands,
    delCommands,
    doneCommands,
    todoCommands,
    doingCommands,
    cancelCommands,
    suspendCommands,
    deadlineCommands,
    waitCommands,
    buildCommands,
    nextCommands,
    quitCommands,
    checkpointCommands,
    cleanCommands,
    failedCommands,
    numberCommands,
    topicCommands,
    allCommands,
    sortedCommands,
  )
where

import Data.List (sort)
import Text.Printf

data Model = Model
  { tasks :: [Task],
    quit :: Bool,
    _error :: Maybe String,
    time :: Integer,
    checkpoint :: Integer,
    doNotBackup :: Bool,
    trash :: [String]
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
  deriving (Show)

data Command
  = NewTask String
  | DeleteTask String
  | SetTaskState TaskState String
  | Deadline String
  | SetNumber String
  | SetTopic String
  deriving (Show)

addCommands :: [String]
addCommands = ["new", "add", "a"]

delCommands :: [String]
delCommands = ["delete", "remove", "d"]

doneCommands :: [String]
doneCommands = ["done"]

todoCommands :: [String]
todoCommands = ["todo"]

doingCommands :: [String]
doingCommands = ["doing", "now", "start"]

cancelCommands :: [String]
cancelCommands = ["cancel"]

suspendCommands :: [String]
suspendCommands = ["suspend"]

deadlineCommands :: [String]
deadlineCommands = ["deadline"]

waitCommands :: [String]
waitCommands = ["wait"]

buildCommands :: [String]
buildCommands = ["building"]

nextCommands :: [String]
nextCommands = ["next"]

quitCommands :: [String]
quitCommands = ["exit", "quit", "q"]

checkpointCommands :: [String]
checkpointCommands = ["checkpoint", "cp"]

cleanCommands :: [String]
cleanCommands = ["clean"]

failedCommands :: [String]
failedCommands = ["failed"]

numberCommands :: [String]
numberCommands = ["setbuildnumber", "sbn"]

topicCommands :: [String]
topicCommands = ["settopic", "topic"]

allCommands :: [String]
allCommands =
  concat
    [ addCommands,
      delCommands,
      doneCommands,
      todoCommands,
      doingCommands,
      cancelCommands,
      suspendCommands,
      deadlineCommands,
      waitCommands,
      buildCommands,
      nextCommands,
      quitCommands,
      checkpointCommands,
      cleanCommands,
      failedCommands,
      numberCommands,
      topicCommands
    ]

sortedCommands :: [String]
sortedCommands = sort allCommands
