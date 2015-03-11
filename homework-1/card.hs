module Card (validate) where

import Data.List

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []           = []
doubleEveryOther [x1]         = [2*x1]
doubleEveryOther (x1:x2:rest) = 2*x1:x2: doubleEveryOther rest

sumDigits :: [Integer] -> Integer
sumDigits = foldl' (+) 0 . concatMap toDigits

validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
