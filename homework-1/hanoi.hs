type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 first second third = [(first, third)]
-- Intuition: 
-- For a 3 disks initial arrangement, we have
--   *     |     |
--  ***    |     |
-- *****   |     |
-- =====#=====#=====
-- Then a major collection of step will be move the disks above
-- the largest disk to the second peg (any peg but the right most peg, 
-- though there is only one choice available here).
--   |     |     |
--   |     *     |
-- *****  ***    |
-- =====#=====#=====
-- Obviously the next step is move the largest disk to the right most peg.
--   |     |     |  
--   |     *     |  
--   |    ***  *****
-- =====#=====#=====
-- Finally we move the n-1 smaller disks from second peg to the right most
-- peg and finished the game.
hanoi number first second third = hanoi (number-1) first third second ++ [(first, third)] ++ hanoi (number-1) second first third

{- Exercise 6 -}
-- The size of solution to tower of hanoi with more than 3 pegs is a unsolved problem.
-- Right now for 4 pegs, the algorithm i come up with, tho hard coded here, is:
-- first move a number of discs to another pegs, then move the remaining discs in initial pegs to destination, which is essentially like a 3-peg scenario.
-- finally move stack of discs we moved first to the destination.
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n first second third forth
  | n < 3     = hanoi n first second forth
  | n == 3    = [(first, second), (first, third), (first, forth), (third, forth), (second, forth)]
  | n < 10     = hanoi4 (n-3) first forth third second ++ hanoi 3 first third forth ++ hanoi4 (n-3) second first third forth
  | n < 14     = hanoi4 (n-4) first forth third second ++ hanoi 4 first third forth ++ hanoi4 (n-4) second first third forth
  | n < 17     = hanoi4 (n-5) first forth third second ++ hanoi 5 first third forth ++ hanoi4 (n-5) second first third forth
  | otherwise = hanoi4 (n-3) first forth third second ++ hanoi 3 first third forth ++ hanoi4 (n-3) second first third forth 

lengthHanoi :: Integer -> Int
lengthHanoi n = length $ hanoi n "a" "b" "c"

lengthHanoi4 :: Integer -> Int
lengthHanoi4 n = length $ hanoi4 n "a" "b" "c" "d"
