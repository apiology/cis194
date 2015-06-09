{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseTimestampAndRest :: [String] -> Maybe (TimeStamp, String)
parseTimestampAndRest (timestamp:messages) = Just ((read timestamp), (unwords messages))
parseTimestampAndRest _ = Nothing

parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType ("E":severity:rest) = Just ((Error (read severity)), rest)
parseMessageType ("I":rest) = Just (Info, rest)
parseMessageType ("W":rest) = Just (Warning, rest)
parseMessageType _ = Nothing

-- XXX change to do double pattern matching?
parseMessageFromList :: [String] -> LogMessage
parseMessageFromList l = 
    case parseMessageType l of
      Just (messagetype, rest) -> case parseTimestampAndRest rest of
                                  Just (timestamp, message) -> LogMessage messagetype timestamp message
                                  Nothing -> Unknown (unwords l)
      Nothing                  -> Unknown (unwords l)
    
parseMessage :: String -> LogMessage
parseMessage s = parseMessageFromList (words s)

parse :: String -> [LogMessage]
parse file = map parseMessage (lines file)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ newts _) (Node left existing@(LogMessage _ existingts _) right)
  | newts < existingts = Node (insert lm left) existing right
  | otherwise          = Node left existing (insert lm right)
insert (LogMessage _ _ _) (Node _ (Unknown _) _) = error "Found Unknown in tree!"

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ [message] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
  (map toString (inOrder (build (filter important messages))))
  where toString (LogMessage _ _ str) = str
        toString (Unknown _) = error "Shouldn't happen!"
        important (LogMessage (Error level) _ _) = level >= 50
        important _ = False
