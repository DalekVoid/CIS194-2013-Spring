
module Calc where

import ExprT
import Parser

{- Version 1 -}
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add addend adder) = eval addend + eval adder
eval (Mul multiplicand multiplier) = eval multiplicand * eval multiplier

example = Mul (Add (Lit 2) (Lit 3)) (Lit 4)
evalExample = eval example

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

strExample1 = "(2+3)*4"
strExample2 = "2+3*4"
strExample3 = "2+3*"

{- Version 2 -}

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (&&)
  mul = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . (flip mod 7) 
  add (Mod7 addend) (Mod7 adder) = Mod7 ((addend + adder) `mod` 7)
  mul (Mod7 multiplicand) (Mod7 multiplier) = Mod7 ((multiplicand * multiplier) `mod` 7)

classExample = mul (add (lit 2) (lit 3)) (lit 4) :: ExprT

-- classExampleUntyped = mul (add (lit 2) (lit 3)) (lit 4)

reify :: ExprT -> ExprT
reify = id

reifyExample = reify $ mul (add (lit 2) (lit 3)) (lit 4)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax 
testSat     = testExp :: Maybe Mod7
