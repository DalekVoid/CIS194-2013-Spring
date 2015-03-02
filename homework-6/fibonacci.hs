{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed g seed = Cons seed (streamFromSeed g (g seed))

interleavesStream :: Stream a -> Stream a -> Stream a
interleavesStream (Cons x xs) yys = Cons x (interleavesStream yys xs)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- ///////0//
-- ///0///1//
-- /0/1/0/1/0
-- 0101010101
-------------
-- 0102010301

zipStream :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStream f (Cons x xs) (Cons y ys) = Cons (f x y) (zipStream f xs ys)

ruler :: Stream Integer
ruler = interleavesStream (streamRepeat 0) (zipStream (+) ruler (streamRepeat 1))

largestPowerDivisible :: Integer -> Integer -> Integer
largestPowerDivisible n x
  | x `mod` n == 0 = 1 + largestPowerDivisible n (x `div` n)
  | otherwise      = 0

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)

  negate = streamMap negate

  (Cons x xs) + (Cons y ys) = Cons (x+y) (xs + ys)

  xxs@(Cons x xs) * yys@(Cons y ys) = Cons (x * y) (streamMap (*x) ys + xs * yys)

instance Fractional (Stream Integer) where
  xxs@(Cons x xs) / yys@(Cons y ys) = Cons (x`div`y) (streamMap (*(1 `div` y)) (xs - (xxs / yys) * ys))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix a = Matrix a a a a

{- [ a b ] * [ e f ] = [ ae*bg af*bh ]
 - [ c d ]   [ g h ]   [ ce*dg cf*dh ]
 -}
instance Num (Matrix Integer) where
  (Matrix a b c d) * (Matrix e f g h) = Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

takeTopLeft :: Matrix Integer -> Integer
takeTopLeft (Matrix a b c d) = a

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = takeTopLeft $ (^(n-1)) (Matrix 1 1 1 0)
