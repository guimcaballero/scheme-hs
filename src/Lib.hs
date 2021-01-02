module Lib
    ( main
    ) where

import System.Environment
import System.IO
import Control.Monad.Except

import Types
import Primitives
import Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args

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

runOne :: [String] -> IO ()
runOne args = do
  env <-
    primitiveBindings >>=
    flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String $ head args])) >>=
    hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme>=> ") . evalAndPrint
