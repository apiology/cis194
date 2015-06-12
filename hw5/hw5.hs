{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (ExprT.Lit a) = a
eval (ExprT.Add e1 e2) = (eval e1) + (eval e2)
eval (ExprT.Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr s = evalM $ parseExp ExprT.Lit ExprT.Add ExprT.Mul s
            where evalM Nothing = Nothing
                  evalM (Just e) = Just $ eval e

class Expr t where
  lit :: Integer -> t
  mul :: t -> t -> t
  add :: t -> t -> t

instance Expr ExprT where
  lit = ExprT.Lit
  mul = ExprT.Mul
  add = ExprT.Add

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)

instance Expr Bool where
  lit i = if i <= 0 then False else True
  mul = (&&)
  add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)

-- “addition” is taken to be the max function, while “multiplication”
-- is the min function
instance Expr MinMax where
  lit i = MinMax i
  mul (MinMax a) (MinMax b) = MinMax (min a b)
  add (MinMax a) (MinMax b) = MinMax (max a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

-- all values should be in the ranage 0 . . . 6, and all arithmetic is
-- done modulo 7; for example, 5 + 3 = 1.

instance Expr Mod7 where
  lit i = Mod7 i
  mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)
  add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr Program where
   lit i = [StackVM.PushI i]
   mul as bs = as ++ bs ++ [StackVM.Mul]
   add as bs = as ++ bs ++ [StackVM.Add]

testProgram = testExp :: Maybe Program

compile :: String -> Maybe Program
compile s = parseExp lit add mul s   

