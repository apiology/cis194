{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char -- so I can run examples with isUpper, etc

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- XXX is this already somewhere?  hoogle it
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (:) <$> p <*> zeroOrMore p <|> pure []
-- zeroOrMore p = p <:> (zeroOrMore p <|> pure []) -- XXX: Figure out if this works

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p
-- oneOrMore p = p <:> zeroOrMore p -- XXX: Figure out if this works

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

ident :: Parser String
ident = satisfy isAlpha <:> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

-- Comb <$> (:) <*> oneOrMore (parseSExpr))
parseComb :: Parser [SExpr]
parseComb = char '(' *> oneOrMore parseSExpr <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = spaces *> ((A <$> parseAtom) <|> (Comb <$> parseComb)) <* spaces
-- parseSExpr = A <$> parseAtom
--  where 
