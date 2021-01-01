module Lib
    ( main
    ) where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

import Parsing
import Types
import Primitives

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "scheme" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val
