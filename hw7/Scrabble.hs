{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i           

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)


score :: Char -> Score
score = scoreLC . toLower

scoreLC :: Char -> Score
scoreLC 'a' = 1
scoreLC 'b' = 3
scoreLC 'c' = 3
scoreLC 'd' = 2
scoreLC 'e' = 1
scoreLC 'f' = 4
scoreLC 'g' = 2
scoreLC 'h' = 4
scoreLC 'i' = 1
scoreLC 'j' = 8
scoreLC 'k' = 5
scoreLC 'l' = 1
scoreLC 'm' = 3
scoreLC 'n' = 1
scoreLC 'o' = 1
scoreLC 'p' = 3
scoreLC 'q' = 10
scoreLC 'r' = 1
scoreLC 's' = 1
scoreLC 't' = 1
scoreLC 'u' = 1
scoreLC 'v' = 4
scoreLC 'w' = 4
scoreLC 'x' = 8
scoreLC 'y' = 4
scoreLC 'z' = 10
scoreLC _ = 0

scoreString :: String -> Score
scoreString s = mconcat $ map score s
