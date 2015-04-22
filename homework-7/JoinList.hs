module JoinList where

import Scrabble

import Sized
import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Show, Eq)

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- Safe list indexing
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
-- if a is not a list, then the only possible value for it is 1
indexJ idx (Single _ a)
  | idx == 0  = Just a
  | otherwise = Nothing
{-
indexJ idx (Single siz a)
  | idx == siz = Just a
  | otherwise  = Nothing
  -}
indexJ idx (Append siz jl1 jl2)
  | idx >= (getSize . size) siz = Nothing
  | idx >= size1 = indexJ (idx - size1) jl2
  | otherwise   = indexJ idx jl1
    where size1 = getSize . size . tag $ jl1


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _)   = Empty
dropJ n (Append siz jl1 jl2)
  | n >= (getSize . size) siz = Empty
  | n >= size1 = dropJ (n - size1) jl2
  | otherwise = dropJ n jl1 +++ jl2
    where size1 = getSize . size . tag $ jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 jl = Empty
takeJ _ Empty = Empty
takeJ _ sing@(Single _ _) = sing
takeJ n jl@(Append siz jl1 jl2)
  | n >= (getSize . size) siz = jl
  | n >= size1 = jl1 +++ takeJ (n - size1) jl2
  | otherwise = takeJ n jl1
    where size1 = getSize . size . tag $ jl1

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

