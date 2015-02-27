{-# OPTIONS_GHC -Wall #-}

module LogAnalysis
  (
    parseMessage,
    parse,
    insert,
    build,
    inOrder,
    whatWentWrong,
    getContent,
    getSeverity
    ) where

import Log
import Data.List(foldl')
import Test.QuickCheck

parseMessage :: String -> LogMessage
parseMessage "" = Unknown ""
parseMessage line@(code:rest)
  | code == 'I' = parseMessageImp Info rest
  | code == 'W' = parseMessageImp Warning rest
  | code == 'E' = parseErrorMessageImp rest
  | otherwise   = Unknown line 

parseMessageImp :: MessageType -> String -> LogMessage
parseMessageImp msgType line = LogMessage msgType time content
  where 
    tokens = words line
    time = read $ concat $ take 1 tokens
    content = unwords $ drop 1 tokens

parseErrorMessageImp :: String -> LogMessage
parseErrorMessageImp line = parseMessageImp (Error level) rest
  where
    tokens = words line
    level = read $ concat $ take 1 tokens
    rest = unwords $ drop 1 tokens

-- Test Cases
testErrorMsg =  parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
testInfoMsg =  parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
testUnknownMsg = parseMessage "This is not in the right format" == Unknown "This is not in the right format"

testCases = [testErrorMsg, testInfoMsg, testUnknownMsg]

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree 
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ time1 _) (Node left msg2@(LogMessage _ time2 _) right)
  | time1 <= time2 = Node (insert msg left) msg2 right
  | otherwise      = Node left msg2 (insert msg right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build list = foldr insert Leaf list

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

getContent :: LogMessage -> String
getContent (Unknown content) = content
getContent (LogMessage _ _ content) = content

getSeverity :: LogMessage -> Int
getSeverity (LogMessage (Error level) _ _) = level
getSeverity _ = -1

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong msgList = map getContent $ filter isSevere sortedList
  where
    sortedList = inOrder $ build msgList
    isSevere = (>=50) . getSeverity
