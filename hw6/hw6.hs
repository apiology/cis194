
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Prelude hiding (foldr, foldl)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i \<- [2..100]])
-- a = array ()

fibs2 :: [Integer]
fibs2 = [0,1] ++ [fibs2 !! (n-1) + fibs2 !! (n-2) | n <- [2..]]

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a rest) = a : streamToList rest

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

streamRepeat :: a -> Stream a
streamRepeat a = Stream a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a rest) = Stream (f a) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed pump seed = Stream seed (streamFromSeed pump (pump seed))

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a s1rest) s2 = Stream a (interleaveStreams s2 s1rest)

-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, ...
-- where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly
-- divides n.

rulerF :: Integer -> Stream Integer
rulerF n = interleaveStreams (streamRepeat n) $ rulerF $ succ n

ruler :: Stream Integer
ruler = rulerF 0

zeros :: Stream Integer
zeros = streamRepeat 0

x :: Stream Integer
x = Stream 0 $ Stream 1 zeros

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Stream a resta) (Stream b restb) =
  Stream (f a b) (streamZipWith f resta restb)

instance Num (Stream Integer) where
  fromInteger n = Stream n zeros
  negate = streamMap negate
  (+) = streamZipWith (+)
  (*) (Stream a0 a') b@(Stream b0 b') = Stream (a0 * b0) ((fromInteger a0 * b') + a'*b)


instance Fractional (Stream Integer) where
  (/) (Stream a0 a') (Stream b0 b')
    = q where q = Stream (a0 `div` b0) (streamMap (`div` b0) (a' - q*b'))

fibs3 :: Stream Integer
fibs3 = x/(1-x-x^2)

data Matrix a = Matrix a a a a

instance Num (Matrix Integer) where
  (*) (Matrix a b c d) (Matrix e f g h)
    = Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

f :: Matrix Integer
f = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = b where Matrix a b c d = f^n

