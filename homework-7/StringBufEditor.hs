module Main where

import JoinList
import Sized
import Scrabble
import StringBuffer
import Editor

{-
main = runEditor editor $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
         -}
main = runEditor editor (Empty :: JoinList (Score, Size) String)
