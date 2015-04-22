module Scrabble where

import Data.List (foldl1')
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Control.Arrow (first)
import qualified Data.Map as M

letterScore :: M.Map Char Int
letterScore = M.fromList (uppercaseScore ++ lowercaseScore)
  where
    uppercaseScore = concat [[(c, score) | c <- characters] | (characters, score) <- charactersScore]
    lowercaseScore = map (first toLower) uppercaseScore
    charactersScore = [ ("EAIONRTLSU", 1)
                      , ("DG", 2)
                      , ("BCMP", 3)
                      , ("FHVWY", 4)
                      , ("K", 5)
                      , ("JX", 8)
                      , ("QZ", 10)
                      ]

data Score = Score Int
  deriving (Show, Eq)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty = Score 0
  (Score s1) `mappend` (Score s2) = Score (s1 + s2)


score :: Char -> Score
score char = Score . fromMaybe 0 $ M.lookup char letterScore

scoreString :: String -> Score
scoreString = mconcat . map score
