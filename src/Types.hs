module Types where

import Numeric ()
import Data.Complex
import Data.Ratio
import Data.Array

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | Vector (Array Int LispVal)
  -- deriving (Show)

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char a) = [a]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList a as) = "(" ++ unwordsList a ++ " . " ++ showVal as ++ ")"
showVal (Ratio rat) = show (numerator rat) ++ "/" ++ show (denominator rat)
showVal (Complex (a :+ b)) = show a ++ "+" ++ show b ++ "i"
showVal (Vector arr) = "#(" ++ unwordsList (elems arr) ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
