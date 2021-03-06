import Data.List
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

my_fun1 :: [Integer] -> Integer
my_fun1 = foldr (*) 1 . map (flip subtract 2) . filter even

my_fun2 :: Integer -> Integer
my_fun2 = sum . filter even . takeWhile (>0) . iterate (\x -> case x of
                                                              1 -> 0
                                                              _ | even x -> x `div` 2
                                                                | otherwise -> 3 * x + 1)
