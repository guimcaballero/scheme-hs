module Primitives where

import Types
import Environment
import Parsing

import qualified Data.Text as T

import System.IO
import Control.Monad.Except
import Data.Bifunctor
import Data.Maybe (isNothing)
import Text.ParserCombinators.Parsec hiding (spaces)

-------------------------
-- Reading
-------------------------

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr:: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr
readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)


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

-- Load
eval env (List [Atom "load", String filename]) =
     load filename >>= fmap last . mapM (eval env)

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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (second IOFunc) ioPrimitives
                                               ++ map (second PrimitiveFunc) primitives)

makeFunc :: Maybe T.Text -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map (T.pack . show) params) varargs body env
makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing
makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . T.pack . show


-------------------------
-- IO Primitive functions
-------------------------

ioPrimitives :: [(T.Text, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc params            = throwError $ NumArgs 2 params -- TODO This is actually a min

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = Port <$> liftIO (openFile (T.unpack filename) mode)
makePort _    [a]               = throwError $ TypeMismatch "port" a
makePort _    params            = throwError $ NumArgs 1 params

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort [a]         = throwError $ TypeMismatch "port" a
closePort params      = throwError $ NumArgs 1 params

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc [a]         = throwError $ TypeMismatch "port" a
readProc params      = throwError $ VariableNumArgs [0, 1] params

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc [_, a]           = throwError $ TypeMismatch "port" a
writeProc params           = throwError $ VariableNumArgs [1, 2] params

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = String . T.pack <$> liftIO (readFile $ T.unpack filename)
readContents [a]               = throwError $ TypeMismatch "string" a
readContents params            = throwError $ NumArgs 1 params

load :: T.Text -> IOThrowsError [LispVal]
load filename = liftIO (readFile $ T.unpack filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename
readAll [a]               = throwError $ TypeMismatch "string" a
readAll params            = throwError $ NumArgs 1 params

-------------------------
-- Primitive functions
-------------------------

primitives :: [(T.Text, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("not", unaryOp not')
  , ("symbol?", unaryOp symbolp)
  , ("string?", unaryOp stringp)
  , ("number?", unaryOp numberp)
  , ("bool?", unaryOp boolp)
  , ("list?", unaryOp listp)
  , ("symbol->string", unaryOp symbol2string)
  , ("string->symbol", unaryOp string2symbol)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("string-length", stringLen)
  , ("string-ref", stringRef)
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  ]

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError T.Text
unpackStr (String s) = return s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool



numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _           []  = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = Number . foldl1 op <$> mapM unpackNum params


unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v
unaryOp _ params = throwError $ NumArgs 1 params

not', symbolp, numberp, stringp, boolp, listp, symbol2string, string2symbol :: LispVal -> LispVal

not' (Bool x) = (Bool . not) x
not' _ = Bool False

symbolp (Atom _)   = Bool True
symbolp _          = Bool False

numberp (Number _) = Bool True
numberp _          = Bool False

stringp (String _) = Bool True
stringp _          = Bool False

boolp   (Bool _)   = Bool True
boolp   _          = Bool False

listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbol2string (Atom s)   = String s
symbol2string _          = String ""

string2symbol (String s) = Atom s
string2symbol _          = Atom ""

stringLen :: [LispVal] -> ThrowsError LispVal
stringLen [(String s)] = Right $ Number $ fromIntegral $ T.length s
stringLen [notString]  = throwError $ TypeMismatch "string" notString
stringLen badArgList   = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String s), (Number k)]
    | T.length s < k' + 1 = throwError $ Default "Out of bound error"
    | otherwise         = Right $ Char $ T.index s $ fromIntegral k
    where k' = fromIntegral k
stringRef [(String _), notNum] = throwError $ TypeMismatch "number" notNum
stringRef [notString, _]       = throwError $ TypeMismatch "string" notString
stringRef badArgList           = throwError $ NumArgs 2 badArgList


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum
strBoolBinop :: (T.Text -> T.Text -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool


car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [a, b] = return $ Bool $ a == b
eqv badArg = throwError $ NumArgs 2 badArg
