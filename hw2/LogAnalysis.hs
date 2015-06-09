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

parseMessageFromList :: [String] -> LogMessage
parseMessageFromList l = 
    case parseMessageType l of
      Just (messagetype, rest) -> case parseTimestampAndRest rest of
                                  Just (timestamp, message) -> LogMessage messagetype timestamp message
                                  Nothing -> Unknown (unwords l)
      Nothing                  -> Unknown (unwords l)
    
parseMessage :: String -> LogMessage
parseMessage s = parseMessageFromList (words s)
