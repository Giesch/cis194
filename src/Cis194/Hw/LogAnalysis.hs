{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

parseError :: String -> String -> [String] -> LogMessage
parseError severity time msg =
  LogMessage (Error (read severity)) (read time) (unwords msg)

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  "E" : severity : time : rest -> parseError severity time rest
  "I" : time : rest -> buildMessage Info time rest
  "W" : time : rest -> buildMessage Warning time rest
  _ -> Unknown s

buildMessage :: MessageType -> String -> [String] -> LogMessage
buildMessage typ time msg = LogMessage typ (read time) (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert Unknown{} tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left current right) =
  if (timestamp msg) >= (timestamp current)
  then Node left current (insert msg right)
  else Node (insert msg left) current right

timestamp :: LogMessage -> TimeStamp
timestamp (LogMessage _ t _) = t
timestamp (Unknown _) = error "Unknown message has no timestamp"

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree = go tree []
  where
    go (Node left msg right) = go left . (msg:) . go right
    go Leaf = id

message :: LogMessage -> String
message (Unknown s) = s
message (LogMessage _ _ s) = s

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error s) _ _) = s >= 50
isSevere _ = False

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . filter isSevere . inOrder . build
