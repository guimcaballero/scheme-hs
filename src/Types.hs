module Types where

import Numeric ()
import Data.Complex
import Data.Ratio
import Data.Array

import Text.ParserCombinators.Parsec (ParseError)


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
  deriving (Eq)

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


data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | OutOfBounds
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError OutOfBounds                   = "Out of bounds error"
showError (Default string)              = string

instance Show LispError where
  show = showError

type ThrowsError = Either LispError
