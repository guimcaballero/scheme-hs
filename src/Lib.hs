module Lib
    ( main
    ) where

import System.Environment
import System.IO
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

import Parsing
import Types
import Primitives

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program takes only 0 or 1 argument"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "Extracting from a Left"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> String
evalString expr = extractValue $ trapError (show <$> (readExpr expr >>= eval))

evalAndPrint :: String -> IO ()
evalAndPrint expr = putStrLn $ evalString expr

until_ :: Monad m => (String -> Bool) -> m String -> (String -> m ()) -> m ()
until_ endPred prompt action = do
  result <- prompt
  if null result
    then until_ endPred prompt action
    else if endPred result
           then return ()
           else action result >> until_ endPred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Scheme>=> ") evalAndPrint
