{-# LANGUAGE FlexibleContexts #-}

module Primitives where

import Types
import Environment
import Parsing
import Evaluation

import qualified Data.Text as T

import System.IO
import Control.Monad.Except
import Data.Bifunctor

import Data.Complex
import Data.Ratio
import Data.Fixed


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (second IOFunc) ioPrimitives
                                               ++ map (second PrimitiveFunc) primitives
                                               ++ primitiveVariables )

-------------------------
-- Default variables
-------------------------

primitiveVariables :: [(T.Text, LispVal)]
primitiveVariables = [("stdlib", String "lib/stdlib.scm")]

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

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename
readAll [a]               = throwError $ TypeMismatch "string" a
readAll params            = throwError $ NumArgs 1 params

-------------------------
-- Primitive functions
-------------------------

primitives :: [(T.Text, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numAdd)
  , ("-", numSub)
  , ("*", numMul)
  , ("/", numDiv)
  , ("mod", numMod)

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

  , ("=", numBoolBinopEq)
  , (">", numBoolBinopGt)
  , (">=", numBoolBinopGte)
  , ("<", numBoolBinopLt)
  , ("<=", numBoolBinopLte)

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

-- - Begin GenUtil - http://repetae.net/computer/haskell/GenUtil.hs
foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM f v (x : xs) = (f v x) >>= \ a -> foldlM f a xs
foldlM _ v [] = return v

foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldl1M f (x : xs) = foldlM f x xs
foldl1M _ _ = error "Unexpected error in foldl1M"
-- end GenUtil

-- |Accept two numbers and cast one of them to the appropriate type, if necessary
numCast' :: (LispVal, LispVal) -> ThrowsError (LispVal, LispVal)
numCast' (a@(Number _), b@(Number _)) = return $ (a, b)
numCast' (a@(Float _), b@(Float _)) = return $ (a, b)
numCast' (a@(Ratio _), b@(Ratio _)) = return $ (a, b)
numCast' (a@(Complex _), b@(Complex _)) = return $ (a, b)
numCast' ((Number a), b@(Float _)) = return $ (Float $ fromInteger a, b)
numCast' ((Number a), b@(Ratio _)) = return $ (Ratio $ fromInteger a, b)
numCast' ((Number a), b@(Complex _)) = return $ (Complex $ fromInteger a, b)
numCast' (a@(Float _), (Number b)) = return $ (a, Float $ fromInteger b)
numCast' (a@(Float _), (Ratio b)) = return $ (a, Float $ fromRational b)
numCast' ((Float a), b@(Complex _)) = return $ (Complex $ a :+ 0, b)
numCast' (a@(Ratio _), (Number b)) = return $ (a, Ratio $ fromInteger b)
numCast' ((Ratio a), b@(Float _)) = return $ (Float $ fromRational a, b)
numCast' ((Ratio a), b@(Complex _)) = return $ (Complex $ (fromInteger $ numerator a) / (fromInteger $ denominator a), b)
numCast' (a@(Complex _), (Number b)) = return $ (a, Complex $ fromInteger b)
numCast' (a@(Complex _), (Float b)) = return $ (a, Complex $ b :+ 0)
numCast' (a@(Complex _), (Ratio b)) = return $ (a, Complex $ (fromInteger $ numerator b) / (fromInteger $ denominator b))
numCast' (a, b) = case a of
               Number _ -> doThrowError b
               Float _ -> doThrowError b
               Ratio _ -> doThrowError b
               Complex _ -> doThrowError b
               _ -> doThrowError a
  where doThrowError num = throwError $ TypeMismatch "number" num

-- |Accept two numbers and cast one of them to the appropriate type, if necessary
numCast :: [LispVal] -> ThrowsError LispVal
numCast [a, b] = do
  (a', b') <- numCast' (a, b)
  pure $ List [a', b']
numCast _ = throwError $ Default "Unexpected error in numCast"

-- |Add the given numbers
numAdd :: [LispVal] -> ThrowsError LispVal
numAdd [] = return $ Number 0
numAdd aparams = do
  foldl1M (\ a b -> doAdd =<< (numCast [a, b])) aparams
  where doAdd (List [(Number a), (Number b)]) = return $ Number $ a + b
        doAdd (List [(Float a), (Float b)]) = return $ Float $ a + b
        doAdd (List [(Ratio a), (Ratio b)]) = return $ Ratio $ a + b
        doAdd (List [(Complex a), (Complex b)]) = return $ Complex $ a + b
        doAdd _ = throwError $ Default "Unexpected error in +"

-- |Subtract the given numbers
numSub :: [LispVal] -> ThrowsError LispVal
numSub [] = throwError $ NumArgs 1 []
numSub [Number n] = return $ Number $ -1 * n
numSub [Float n] = return $ Float $ -1 * n
numSub [Ratio n] = return $ Ratio $ -1 * n
numSub [Complex n] = return $ Complex $ -1 * n
numSub aparams = do
  foldl1M (\ a b -> doSub =<< (numCast [a, b])) aparams
  where doSub (List [(Number a), (Number b)]) = return $ Number $ a - b
        doSub (List [(Float a), (Float b)]) = return $ Float $ a - b
        doSub (List [(Ratio a), (Ratio b)]) = return $ Ratio $ a - b
        doSub (List [(Complex a), (Complex b)]) = return $ Complex $ a - b
        doSub _ = throwError $ Default "Unexpected error in -"

-- |Multiply the given numbers
numMul :: [LispVal] -> ThrowsError LispVal
numMul [] = return $ Number 1
numMul aparams = do
  foldl1M (\ a b -> doMul =<< (numCast [a, b])) aparams
  where doMul (List [(Number a), (Number b)]) = return $ Number $ a * b
        doMul (List [(Float a), (Float b)]) = return $ Float $ a * b
        doMul (List [(Ratio a), (Ratio b)]) = return $ Ratio $ a * b
        doMul (List [(Complex a), (Complex b)]) = return $ Complex $ a * b
        doMul _ = throwError $ Default "Unexpected error in *"

-- |Divide the given numbers
numDiv :: [LispVal] -> ThrowsError LispVal
numDiv [] = throwError $ NumArgs 1 []
numDiv [Number 0] = throwError $ DivideByZero
numDiv [Ratio 0] = throwError $ DivideByZero
numDiv [Number n] = return $ Ratio $ 1 / (fromInteger n)
numDiv [Float n] = return $ Float $ 1.0 / n
numDiv [Ratio n] = return $ Ratio $ 1 / n
numDiv [Complex n] = return $ Complex $ 1 / n
numDiv aparams = do
  foldl1M (\ a b -> doDiv =<< (numCast [a, b])) aparams
  where doDiv (List [(Number a), (Number b)])
            | b == 0 = throwError $ DivideByZero
            | (mod a b) == 0 = return $ Number $ div a b
            | otherwise = -- Not an integer
                return $ Ratio $ (fromInteger a) / (fromInteger b)
        doDiv (List [(Float a), (Float b)])
            | b == 0.0 = throwError $ DivideByZero
            | otherwise = return $ Float $ a / b
        doDiv (List [(Ratio a), (Ratio b)])
            | b == 0 = throwError $ DivideByZero
            | otherwise = return $ Ratio $ a / b
        doDiv (List [(Complex a), (Complex b)])
            | b == 0 = throwError $ DivideByZero
            | otherwise = return $ Complex $ a / b
        doDiv _ = throwError $ Default "Unexpected error in /"

-- |Take the modulus of the given numbers
numMod :: [LispVal] -> ThrowsError LispVal
numMod [] = return $ Number 1
numMod aparams = do
  foldl1M (\ a b -> doMod =<< (numCast [a, b])) aparams
  where doMod (List [(Number a), (Number b)]) = return $ Number $ mod' a b
        doMod (List [(Float a), (Float b)]) = return $ Float $ mod' a b
        doMod (List [(Ratio a), (Ratio b)]) = return $ Ratio $ mod' a b
        doMod (List [(Complex _), (Complex _)]) = throwError $ Default "modulo not implemented for complex numbers"
        doMod _ = throwError $ Default "Unexpected error in modulo"

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

-- |Compare a series of numbers using a given numeric comparison
--  function and an array of lisp values
numBoolBinopCompare :: (LispVal
                    -> LispVal -> Either LispError LispVal)
                    -> LispVal -> [LispVal] -> Either LispError LispVal
numBoolBinopCompare cmp n1 (n2 : ns) = do
  (n1', n2') <- numCast' (n1, n2)
  result <- cmp n1' n2'
  case result of
    Bool True -> numBoolBinopCompare cmp n2' ns
    _ -> return $ Bool False
numBoolBinopCompare _ _ _ = return $ Bool True

-- |Numeric equals
numBoolBinopEq :: [LispVal] -> ThrowsError LispVal
numBoolBinopEq [] = throwError $ NumArgs 0 []
numBoolBinopEq (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a == b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Ratio a) (Ratio b) = return $ Bool $ f a b
    cmp (Complex a) (Complex b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in ="

-- |Numeric greater than
numBoolBinopGt :: [LispVal] -> ThrowsError LispVal
numBoolBinopGt [] = throwError $ NumArgs 0 []
numBoolBinopGt (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a > b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Ratio a) (Ratio b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in >"

-- |Numeric greater than equal
numBoolBinopGte :: [LispVal] -> ThrowsError LispVal
numBoolBinopGte [] = throwError $ NumArgs 0 []
numBoolBinopGte (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a >= b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Ratio a) (Ratio b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in >="

-- |Numeric less than
numBoolBinopLt :: [LispVal] -> ThrowsError LispVal
numBoolBinopLt [] = throwError $ NumArgs 0 []
numBoolBinopLt (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a < b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Ratio a) (Ratio b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in <"

-- |Numeric less than equal
numBoolBinopLte :: [LispVal] -> ThrowsError LispVal
numBoolBinopLte [] = throwError $ NumArgs 0 []
numBoolBinopLte (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a <= b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Ratio a) (Ratio b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in <="

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
