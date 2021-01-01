module Lib
    ( main
    ) where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

import Parsing
import Types
import Primitives

import Control.Monad.Except

main :: IO ()
main = do
     args <- getArgs
     let evaled = fmap show $ readExpr (head args) >>= eval
     putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "Extracting from a Left"
