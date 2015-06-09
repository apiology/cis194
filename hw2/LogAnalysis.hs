{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- read
-- lines, words, unwords, take, drop, and (.).)

-- XXX: Really want to combine code that parses the rest here:

parseMessageFromList :: [String] -> LogMessage
parseMessageFromList ("E":severity:timestamp:messages) = LogMessage (Error (read severity)) (read timestamp) (unwords messages)
parseMessageFromList ("W":timestamp:messages) = LogMessage Warning (read timestamp) (unwords messages)
parseMessageFromList ("I":timestamp:messages) = LogMessage Info (read timestamp) (unwords messages)
parseMessageFromList messages = Unknown (unwords messages)

parseMessage :: String -> LogMessage
parseMessage s = parseMessageFromList (words s)
