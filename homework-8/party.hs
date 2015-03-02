{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.Tree
import Data.Monoid
import Data.Ord
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL xs score) = GL (emp:xs) (score+fun)

-- First Implmentation: at current point, integrety of values inside GL doesn't matter anyway
instance Monoid GuestList where
  mempty = GL [] 0
  (GL xxs xScore) `mappend` (GL yys yScore) = GL (xxs++yys) (xScore+yScore)

moreFun :: GuestList -> GuestList -> GuestList
moreFun list1 list2 = case list1 `compare` list2 of
                           GT -> list1
                           EQ -> list1
                           LT -> list2

{-
nameConcat :: String -> Employee -> String
nameConcat str (Emp name _) = str ++ name
-}

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f initialValue (Node entry []) = f entry [initialValue]
treeFold f initialValue (Node entry forest) = f entry (map (treeFold f initialValue) forest)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- nextLevel emp guestList = (mconcat withBoss, glCons emp $ mconcat withoutBoss)
nextLevel emp guestList = (mconcat $ zipWith moreFun withoutBoss withBoss, glCons emp $ mconcat withoutBoss)
  where (withoutBoss, withBoss) = unzip guestList

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun withCEO withoutCEO
  where (withoutCEO, withCEO) = treeFold nextLevel mempty tree

main :: IO ()
main = do
        fileContent <- readFile "company.txt"
        putStrLn . show . maxFun $ read fileContent
