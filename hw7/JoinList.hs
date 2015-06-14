
import Sized
import Data.Monoid

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
tag (Single m a) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ i (Single m a) = if i == 0 then Just a else Nothing
indexJ i (Append m x y)
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
dropJ i Empty = Empty
dropJ i jl@(Single m a) = if i == 0 then jl else Empty
-- dropJ i (Append m l1 l2)
   -- | i <= xsize          = (Append )
   -- | i < (xsize + ysize) = indexJ (i - xsize) y
   -- | otherwise           = Nothing
   -- where xsize = getSize $ size $ tag x
   --       ysize = getSize $ size $ tag y
  
