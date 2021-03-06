{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

emptylist :: Parser [a]
emptylist = pure []

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> emptylist

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

validSExpr1 = "5"
validSExpr2 = "foo3"
validSExpr3 = "(bar (foo) 3 5 874)"
validSExpr4 = "(((lambda x (lambda y (plus x y))) 3) 5)"
validSExpr5 = "(   lots  of   (  spaces   in  )  this ( one ) )"

parseOpen :: Parser Char
parseOpen = char '('

parseClose :: Parser Char
parseClose = char ')'

parseSExpr :: Parser SExpr
parseSExpr =  Comb    <$> (spaces *> parseOpen *> oneOrMore parseSExpr <* spaces <* parseClose)
          <|> (A . I) <$> (spaces *> ident)
          <|> (A . N) <$> (spaces *> posInt)
