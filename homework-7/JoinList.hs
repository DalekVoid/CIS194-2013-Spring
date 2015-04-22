{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Scrabble

import Sized
import Data.Monoid
import Buffer

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

-- Exercise 2
sizeOf :: (Sized b, Monoid b) => JoinList b a -> Int
sizeOf = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ idx _  | idx < 0         = Nothing
indexJ idx jl | idx > sizeOf jl = Nothing
indexJ _ Empty                  = Nothing
indexJ idx (Single _ a)         = Just a
indexJ idx (Append siz jl1 jl2)
  | idx >= size1 = indexJ (idx - size1) jl2
  | otherwise   = indexJ idx jl1
    where size1 = sizeOf jl1


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n _  | n <= 0         = Empty
dropJ n jl | n >= sizeOf jl = Empty
dropJ _ Empty               = Empty
dropJ _ (Single _ _)        = Empty
dropJ n (Append _ jl1 jl2)
  | n >= size1 = dropJ (n - size1) jl2
  | otherwise = dropJ n jl1 +++ jl2
    where size1 = sizeOf jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0          = Empty
takeJ _ Empty               = Empty
takeJ n jl | n >= sizeOf jl = jl
takeJ _ sing@(Single _ _) = sing
takeJ n jl@(Append _ jl1 jl2)
  | n >= size1 = jl1 +++ takeJ (n - size1) jl2
  | otherwise = takeJ n jl1
    where size1 = sizeOf jl1

-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ str) = str
  toString (Append _ jl1 jl2) = toString jl1 ++ toString jl2
  fromString str = foldr (+++) Empty . map fromString' . lines $ str
    where fromString' ln = Single (scoreString ln, Size 1) ln
  line = indexJ
  numLines = sizeOf
  value = getScore . fst . tag
  replaceLine n l b = preceding +++ fromString l +++ following
    where
      splitJoinList n jl = (takeJ n jl, dropJ (n+1) jl)
      (preceding, following) = splitJoinList n b
