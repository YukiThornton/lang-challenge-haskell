{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.Read (readMaybe)

data FailableMessageType = InvalidType
                      | ValidType MessageType

parseMessageType :: [String] -> FailableMessageType
parseMessageType ("I": _) = ValidType Info
parseMessageType ("W": _) = ValidType Warning
parseMessageType ("E": level: _) = ValidType (Error (read level))
parseMessageType _ = InvalidType

parseLogPartsWithType :: MessageType -> [String] -> LogMessage
parseLogPartsWithType messageType whole@(timeStamp:other) = case readMaybe timeStamp of
    Just num -> LogMessage messageType num (unwords other)
    Nothing -> Unknown (unwords whole)
parseLogPartsWithType messageType other = Unknown (unwords (show messageType:other))

parseLogParts :: [String] -> LogMessage
parseLogParts wordList = case parseMessageType wordList of
    ValidType (Error level) -> parseLogPartsWithType (Error level) (drop 2 wordList)
    ValidType messageType -> parseLogPartsWithType messageType (drop 1 wordList)
    InvalidType -> Unknown (unwords wordList)

parseMessage :: String -> LogMessage
parseMessage line = parseLogParts (words line)

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)


compareMessage :: LogMessage -> LogMessage -> Int
compareMessage (LogMessage _ a _) (LogMessage _ b _)
 | a < b = -1
 | otherwise = 1
compareMessage _ _ = 999

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert newMessage Leaf = Node Leaf newMessage Leaf
insert newMessage (Node left message right) = case compareMessage newMessage message of
    1 -> Node left message (insert newMessage right)
    _ -> Node (insert newMessage left) message right

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

sortMessages :: String -> [LogMessage]
sortMessages file = inOrder (build (parse file))

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error level) _ _) = level >= 50
isSevereError _ = False

takeMessageBody :: LogMessage -> String
takeMessageBody (LogMessage _ _ body) = body
takeMessageBody _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map takeMessageBody (inOrder (build (filter isSevereError messages)))