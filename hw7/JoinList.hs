{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Editor
import Sized
import Data.Monoid
import Scrabble
import Buffer

-- "The m parameter will be used to track monoidal annotations to the
--  structure."
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- "The idea is that the annotation at the root of a JoinList will
-- always be equal to the combination of all the annotations on the
-- Single nodes (according to whatever notion of “combining” is
-- defined for the monoid in question)."

-- Empty nodes do not explicitly store an annotation, but we consider
-- them to have an annotation of mempty (that is, the identity element
-- for the given monoid).

-- This means that for balanced join-lists, it takes only O(log n)
-- time to rebuild the annotations after making an edit.

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

sizeOf :: (Sized b, Monoid b) => JoinList b a -> Int
sizeOf l = getSize $ size $ tag l

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a) = if i == 0 then Just a else Nothing
indexJ i (Append _ x y)
  | i < xsize           = indexJ i x
  | i < (xsize + ysize) = indexJ (i - xsize) y
  | otherwise           = Nothing
  where xsize = getSize $ size $ tag x
        ysize = getSize $ size $ tag y

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl@(Single _ _) = if i == 0 then jl else Empty
dropJ i (Append _ l1 l2)
  | i < l1size            = dropJ i l1 +++ l2
  | i == l1size           = l2
  | i < (l1size + l2size) = dropJ (i - l1size) l2
  | otherwise             = Empty
  where l1size = sizeOf l1
        l2size = sizeOf l2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl@(Single _ _) = if i <= 0 then Empty else jl
takeJ i jl@(Append _ l1 l2)
  | i < l1size            = takeJ i l1
  | i == l1size           = jl
  | i < (l1size + l2size) = l1 +++ takeJ (i - l1size) l2
  | otherwise             = jl
  where l1size = sizeOf l1
        l2size = sizeOf l2
  
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Each JoinList 'Single' represents a single line.
--
-- The Size here represents the number of lines.
--
-- The Score is the scrabble score.
fromLine :: String -> JoinList (Score, Size) String
fromLine s = Single (scoreString s, 1) s

instance (Buffer (JoinList (Score, Size) String)) where
  toString jl = mconcat $ jlToList jl
  fromString s = foldl (+++) Empty $ map fromLine $ lines s
  line = indexJ
  replaceLine n ln jl = takeJ n jl +++ fromString ln +++ dropJ (succ n) jl
  numLines = sizeOf
  value jl = getScore $ fst $ tag jl

initialText :: JoinList (Score, Size) String
initialText = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main :: IO ()
main = runEditor editor initialText
