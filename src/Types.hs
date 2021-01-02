{-# Language RecordWildCards #-}

module Types where

import Numeric ()
import Data.Complex
import Data.Ratio
import Data.Array
import Data.IORef

import Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef LispVal)]

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
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String]
      , varargs :: Maybe String
      , body :: [LispVal]
      , closure :: Env
      }

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {..} =
   "(lambda (" ++ unwords (map show params) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Eq LispVal where
  (Atom a) == (Atom b) = a == b
  (List a) == (List b) = a == b
  (DottedList a c) == (DottedList b d) = a == b && c == d
  (Number a) == (Number b) = a == b
  (String a) == (String b) = a == b
  (Bool a) == (Bool b) = a == b
  (Char a) == (Char b) = a == b
  (Float a) == (Float b) = a == b
  (Ratio a) == (Ratio b) = a == b
  (Complex a) == (Complex b) = a == b
  (Vector a) == (Vector b) = a == b
  _ == _ = False

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
