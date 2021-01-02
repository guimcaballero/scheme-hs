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
import Environment

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ head args
    _ -> putStrLn "Program takes only 0 or 1 argument"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ show <$> (liftThrows (readExpr expr) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

until_ :: Monad m => (String -> Bool) -> m String -> (String -> m ()) -> m ()
until_ endPred prompt action = do
  result <- prompt
  if null result
    then until_ endPred prompt action
    else if endPred result
           then return ()
           else action result >> until_ endPred prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Scheme>=> ") . evalAndPrint
