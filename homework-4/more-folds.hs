import Data.List
xor :: [Bool] -> Bool
xor = foldl1' (\x y -> not(x && y) && (x || y))

map' :: (a -> b) -> [a] -> [b]
-- map' f = foldr (\x -> (++) [f x]) []
map' f = foldr (\x acc-> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] ->a
myFoldl f base xs = foldr (flip f) base xs
