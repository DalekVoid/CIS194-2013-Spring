{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Control.Monad (replicateM, liftM2)
import Control.Arrow (first)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

{-
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)
-}

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

------------------------------------------------------------
-- Battlefield
-- 
{-
instance Random Battlefield where
  random = first (\x y -> Battlefield x y) . (randomR (0, 5) :: Int)
  randomR (low, high) = undefined -- first Battlefield . (randomR (low, high) :: Army)

randomBattlefield :: Rand StdGen Battlefield
randomBattlefield = getRandom
-}
type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

printADie :: IO ()
printADie = do
              g <- getStdGen
              print $ evalRand die g

printADie2 :: IO ()
printADie2 = do
             value <- evalRandIO die
             print value

printDice :: Int -> IO ()
printDice n = do
                g <- getStdGen
                print $ evalRand (dice n) g

diePair :: IO (DieValue, DieValue)
diePair = liftM2 (,) (evalRandIO die) (evalRandIO die)

data BattleResult = Victory | Defeat
  deriving (Eq)

battleResult :: Battlefield -> BattleResult
battleResult bf@(Battlefield 1 _) = Defeat
battleResult _                    = Victory

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

{-
battle :: Battlefield -> IO Battlefield
battle bf@(Battlefield 0 _) = return bf
battle bf@(Battlefield 1 _) = return bf
battle bf@(Battlefield _ 0) = return bf
battle (Battlefield attk def) =
  do
    attkValues <- evalRandIO (dice (min 3 (attk-1)))
    defValues <- evalRandIO (dice (min 2 def))
    let result = mapTuple length $ partition (/=LT) $ zipWith compare (sort attkValues) (sort defValues)
    return $ Battlefield (attk - fst result) (def - snd result)

invade :: Battlefield -> IO Battlefield
invade bf@(Battlefield 0 _) = return bf
invade bf@(Battlefield 1 _) = return bf
invade bf@(Battlefield _ 0) = return bf
-- invade bf = (battle bf) >>= \result ->
--             invade result
invade bf = do
              result <- battle bf
              invade result

successProb :: Battlefield -> IO Double
successProb bf = do
                  results <- replicateM 1000 (invade bf)
                  return $ fromIntegral (length . filter (==Victory) $ map battleResult results)/1000.0
                  -}

------------------------------------------------------------
battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield 0 _) = return bf
battle bf@(Battlefield 1 _) = return bf
battle bf@(Battlefield _ 0) = return bf
battle (Battlefield attk def) =
  do
    attkValues <- dice (min 3 (attk-1))
    defValues <- dice (min 2 def)
    let result = mapTuple length $ partition (/=LT) $ zipWith compare (sort attkValues) (sort defValues)
    return $ Battlefield (attk - fst result) (def - snd result)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield 0 _) = return bf
invade bf@(Battlefield 1 _) = return bf
invade bf@(Battlefield _ 0) = return bf
-- invade bf = (battle bf) >>= \result ->
--             invade result
invade bf = do
              result <- battle bf
              invade result

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
                  results <- replicateM 1000 (invade bf)
                  return $ fromIntegral (length . filter (==Victory) $ map battleResult results)/1000.0
