module Lecture where

import Control.Applicative
import Data.List

(*>)       :: Applicative f => f a -> f b -> f b
(*>) _ b = b
mapA       :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f l = sequenceA $ map f l
sequenceA  :: Applicative f => [f a] -> f [a]
sequenceA = foldl' g (pure [])
  where g = flip $ liftA2 (:)
replicateA :: Applicative f => Int -> f a -> f [a]
-- replicateA 0 _ = pure []
-- replicateA n c = liftA2 (:) c $ replicateA (n - 1) c
replicateA n c = sequenceA $ replicate n c
