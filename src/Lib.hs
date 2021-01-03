module Lib
    ( main
    ) where

import System.Environment
import qualified System.Console.Haskeline as HL
import System.IO
import Control.Monad.Except

import qualified Data.Text as T

import Types
import Primitives
import Environment
import Parsing
import Evaluation

main :: IO ()
main = do
  args <- getArgs
  env <- primitiveBindings

  run args env


run :: [String] -> Env -> IO ()
run [] env = runRepl env
run ["help"] _ = help
run [filename] env = runOne env filename
run ("repl":filename:args) env = do
  env' <- flip bindVars [("args", List $ map (String . T.pack) $ drop 1 args)] env
  runOne env' filename
  runRepl env'
run (filename:args) env = do
  env' <- flip bindVars [("args", List $ map (String . T.pack) $ drop 1 args)] env
  runOne env' filename

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ show <$> (liftThrows (readExpr expr) >>= eval env)

runOne :: Env -> String -> IO ()
runOne env filename = do
  runIOThrows (show <$> eval env (List [Atom "load", String $ T.pack filename])) >>=
    hPutStrLn stderr

-- runRepl :: IO ()
-- runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme>>= ") . evalAndPrint
-- |Start the REPL (interactive interpreter)
runRepl :: Env -> IO ()
runRepl env' = do
    HL.runInputT HL.defaultSettings (loop env')
    where
        -- Main REPL loop
        loop :: Env -> HL.InputT IO ()
        loop env = do
            minput <- HL.getInputLine "Scheme>>= "
            case minput of
                Nothing -> return ()
                Just i -> do
                  case i of
                    "quit" -> return ()
                    "" -> loop env -- ignore inputs of just whitespace
                    input -> do
                        inputLines <- getMultiLine [input]
                        let input' = unlines inputLines
                        result <- liftIO $ evalString env input'
                        if null result
                           then loop env
                           else do HL.outputStrLn result
                                   loop env

        -- Read another input line, if necessary
        getMultiLine previous = do
          if test previous
            then do
              mb_input <- HL.getInputLine ""
              case mb_input of
                Nothing -> return previous
                Just input -> getMultiLine $ previous ++ [input]
            else return previous

        -- Check if we need another input line
        -- This just does a bare minimum, and could be more robust
        test ls = False
        -- do
        --   let cOpen  = LSU.countAllLetters '(' ls
        --       cClose = LSU.countAllLetters ')' ls
        --   cOpen > cClose

help :: IO ()
help = do
  putStrLn "Welcome to Scheme!"
  putStrLn "scheme is a scheme interpreter."
  putStrLn ""
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  stack run                 Start the scheme REPL"
  putStrLn ""
  putStrLn "  stack run help            Display this help information"
  putStrLn ""
  putStrLn "  stack run [file]          Execute the file"
  putStrLn ""
  putStrLn "  stack run repl [file]     Execute the file and open a REPL in the same environment"
