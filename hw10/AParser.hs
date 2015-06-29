{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

-- a functor would map this function onto the "contents" of the
-- parser, and return another Parser that represents that function
-- having been run.

-- Remember,
-- class Functor f where
--  fmap :: (a -> b) -> f a -> f b

-- assuming we're parsing Strings to Integers...
instance Functor Parser where
  -- g :: Integer -> b
  -- p :: Parser Integer
  -- returns: Parser b
  fmap f (Parser g) = Parser h
    where h s = first f <$> g s

-- class Functor f => Applicative f where
--  pure  :: a -> f a -- "you can create a new one"
--   (<*>) :: f (a -> b) -> f a -> f b -- given a container that holds a function, you an apply it

instance Applicative Parser where
  -- pure a represents the parser which consumes no input and
  -- successfully returns a result of a.
  pure a = Parser f
    where
      f s = Just (a, s)
  --  p1 <*> p2 represents the parser which first runs p1 (which will
  --  consume some input and produce a function), then passes the
  --  remaining input to p2 (which consumes more input and produces
  --  some value), then returns the result of applying the function to
  --  the value. However, if either p1 or p2 fails then the whole
  --  thing should also fail (put another way, p1 <*> p2 only succeeds
  --  if both p1 and p2 succeed).
  (<*>) p1 p2 = Parser f
    where
      f s = case runParser p1 s of
             Just (g, restOfS) -> first g <$> runParser p2 restOfS
             Nothing -> Nothing


abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair =  (\a _ b -> [a,b]) <$> posInt <*> char ' ' <*> posInt

-- class Applicative f => Alternative f where
--  empty :: f a
--  (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) p1 p2 = Parser f
    where f s = runParser p1 s <|> runParser p2 s

uppercase :: Parser Char
uppercase = satisfy isUpper

ignore :: Parser a -> Parser ()
ignore p = const () <$> p
      
intOrUppercase :: Parser ()
intOrUppercase = ignore posInt <|> ignore uppercase
