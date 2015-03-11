{-# LANGUAGE MultiWayIf #-}
module Golf where

import Data.List


skip :: [a] -> Int -> Int -> [a]
skip [] _ _ = []
skip (x:xs) current n
  | current `mod` n == 0 = x : skip xs (current+1) n
  | otherwise            = skip xs (current+1) n

skips :: [a] -> [[a]]
skips line = [ skip line 1 n | n <- [1..length line]]

-- zipWith (\x y -> if k==1 -> ) take (length list )interspersed 1&0 list  


{-
localOrdering :: [Integer] -> [Ord]
localOrdering list = [LT] ++ localOrderingImp list ++ [LT]

localOrderingImp :: [Integer] -> [Ord]
localOrderingImp list = zipWith compare list (tail list) 
-}

localMaxima :: [Integer] -> [Integer]
localMaxima list = concat $ zipWith3 maxima (maxInt:list) list (drop 1 list ++ [maxInt]) 
  where
    maxima = \x y z -> if | y > z && y > x -> [y]
                          | otherwise      -> [ ]
    maxInt = 999

test_localMaxima_1 = localMaxima [2,9,5,6,1] == [9,6]
test_localMaxima_2 = localMaxima [2,3,4,1,5] == [4]
test_localMaxima_3 = localMaxima [1,2,3,4,5] == []

testCases = [test_localMaxima_1, test_localMaxima_2, test_localMaxima_3]

appendTo :: [String] -> Integer -> [String]
appendTo bars@(b:bs) i
  | i == 1 = ("*" ++  b) : bs
  | otherwise  = b : appendTo bs (i-1)

bars :: [Integer] -> [String]
bars = foldl' appendTo (replicate 9 [])

spaceFilledBars :: [String] -> [String]
spaceFilledBars bars = map (\bar -> replicate (maxLen - length bar) ' ' ++ bar) bars
  where maxLen= maximum $ map length bars

histogram :: [Integer] -> String
histogram = (++ "=========\n123456789\n") . unlines . transpose . spaceFilledBars . bars

data_1 = [1,4,5,4,6,6,3,4,2,4,9]
