{-# Language FlexibleContexts #-}

module Evaluation where

import Types
import Environment
import Parsing

import qualified Data.Text as T

import Control.Monad.Except
import Data.Maybe (isNothing)

-------------------------
-- Evaluation
-------------------------

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(Number _)  = return val
eval _ val@(String _)  = return val
eval _ val@(Bool _)    = return val
eval _ val@(Char _)    = return val
eval _ val@(Float _)   = return val
eval _ val@(Ratio _)   = return val
eval _ val@(Complex _) = return val
eval _ val@(Vector _)  = return val
eval _ (List [Atom "quote", val]) = return val
eval env (Atom atom) = getVar env atom

-- Control flow
eval env (List [Atom "if", check, a, b]) =
     do result <- eval env check
        case result of
             Bool True -> eval env a
             Bool False -> eval env b
             _ -> throwError $ TypeMismatch "boolean" check
eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clause in cond expression: " form
  else case head clauses of
    List [Atom "else", expr] -> eval env expr
    List [test, expr]        -> eval env $ List [Atom "if",
                                             test,
                                             expr,
                                             List (Atom "cond" : tail clauses)]
    _ -> throwError $ BadSpecialForm "ill-formed cond expression: " form
eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clause in case expression: " form
  else case head clauses of
    List (Atom "else" : exprs) -> last <$> mapM (eval env) exprs
    List ((List datums) : exprs) -> do
      result <- eval env key
      equality <- mapM (\x -> liftThrows $ eqv [result, x]) datums
      if Bool True `elem` equality
        then last <$> mapM (eval env) exprs
        else eval env $ List (Atom "case" : key : tail clauses)
    _                     -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "begin", rest]) = evalBody env rest
eval env (List ((Atom "begin"):rest)) = evalBody env $ List rest

eval env letExpr@(List [Atom "let", List pairsList, expr]) = do
  pairs <- mapM doExtract pairsList

  env' <- liftIO $ bindVars env pairs
  eval env' expr
  where
    doExtract (List [Atom a, b]) = do
      val <- eval env b
      return (a, val)
    doExtract _ = throwError $ BadSpecialForm "let syntax expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)" letExpr
eval _ expr@(List (Atom "let":_) ) = throwError $ BadSpecialForm "let function expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)" expr

-- Load
eval env (List [Atom "load", form]) = do
  path' <- eval env form
  path <- (liftThrows . unpackStr) path'
  load path >>= fmap last . mapM (eval env)

     -- load (eval env form) >>= fmap last . mapM (eval env)

-- Set variable
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var

-- Define variable
eval env (List [Atom "df", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var

-- Define functions
eval env (List (Atom "df" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "df" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var

-- Lambdas
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body

-- Function application
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm



-------------------------
-- Function application
-------------------------

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply Func{..} args =
      if num params /= num args && isNothing varargs
         then throwError $ NumArgs (num params) args
         else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = last <$> mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
                Nothing -> return env
apply _ _ = undefined

eqv :: [LispVal] -> ThrowsError LispVal
eqv [a, b] = return $ Bool $ a == b
eqv badArg = throwError $ NumArgs 2 badArg

makeFunc :: Maybe T.Text -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map (T.pack . show) params) varargs body env
makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing
makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . T.pack . show

evalBody :: Env -> LispVal -> IOThrowsError LispVal
evalBody env (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval env defExpr
  env' <- liftIO $ bindVars env [(var, evalVal)]
  eval env' rest
evalBody env (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval env defExpr
  env' <- liftIO $ bindVars env [(var, evalVal)]
  evalBody env' $ List rest
evalBody env x = eval env x

load :: T.Text -> IOThrowsError [LispVal]
load filename = liftIO (readFile $ T.unpack filename) >>= liftThrows . readExprList




getEven :: [t] -> [t]
getEven [] = []
getEven (x:xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (_:xs) = getEven xs

extractVar :: LispVal -> IOThrowsError T.Text
extractVar (Atom atom) = return atom
extractVar n = throwError $ TypeMismatch "atom" n
