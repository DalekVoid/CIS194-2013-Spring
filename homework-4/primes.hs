import Data.List

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1).(*2)) $ [1..n] \\ (map (\(x, y) -> x + y + 2 * x * y) $ (cartProd [1..n] [1..n]))
