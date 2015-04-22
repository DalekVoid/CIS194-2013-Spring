module Main where

import JoinList
import Sized
import Scrabble
import StringBuffer
import Editor
import Buffer

-- main = runEditor editor $ unlines
main = runEditor editor $
  (fromString "Ten Principles for Good Design" :: JoinList (Score, Size) String) +++
  fromString "Good design is innovative." +++
  fromString "Good design makes a product useful" +++
  fromString "Good design is unobstrusive" +++
  fromString "Good design makes a product understandable" +++
  fromString "Good design is asethetic" +++
  fromString "Good design is honest" +++
  fromString "Good design is thorough down to the last detail" +++
  fromString "Good design is enviornmentally friendly" +++
  fromString "Good design is as little design as possible" +++
  fromString "Good design is long-lasting"

  
{-
main = runEditor editor $ ((foldl1 (+++) . map fromString
         [ "this buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]) :: JoinList (Score, Size) String)
         -}
