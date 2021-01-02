module Parsing where

import Prelude hiding (negate)

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Complex
import Data.Ratio
import Data.Array

import qualified Data.Text as T

import Types

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  _ <- char '"'
  return $ String $ T.pack x

escapedChars :: Parser Char
escapedChars = do
  _ <- char '\\'
  x <- oneOf "\\\"nrt"
  return $
    case x of
      '\\' -> x
      '"' -> x
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      _ -> error "Should not be possible"

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Atom $ T.pack (first:rest)

parseChar :: Parser LispVal
parseChar = do
  _ <- try $ string "#\\"
  value <-
    try (string "newline" <|> string "space") <|> do
      x <- anyChar
      notFollowedBy alphaNum
      return [x]
  return $
    Char $
    case value of
      "space" -> ' '
      "newline" -> '\n'
      _ -> head value

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

negate :: Num a => String -> a -> a
negate "-" num = -num
negate "+" num = num
negate "" num  = num
negate _ _ = undefined

parseDecimal1 :: Parser LispVal
parseDecimal1 = do
  sign <- many (oneOf "-+")
  x <- many1 digit
  (return . Number . negate sign . read) x

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
  _ <- try $ string "#d"
  sign <- many (oneOf "-+")
  x <- many1 digit
  (return . Number . negate sign . read) x

parseHex :: Parser LispVal
parseHex = do
  _ <- try $ string "#x"
  sign <- many (oneOf "-+")
  x <- many1 hexDigit
  return $ Number $ negate sign (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
  _ <- try $ string "#o"
  sign <- many (oneOf "-+")
  x <- many1 octDigit
  return $ Number $ negate sign (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
  _ <- try $ string "#b"
  sign <- many (oneOf "-+")
  x <- many1 (oneOf "10")
  return $ Number $ negate sign (bin2dig x)

oct2dig :: (Eq a, Num a) => String -> a
oct2dig x = fst $ head (readOct x)
hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ head (readHex x)
bin2dig :: String -> Integer
bin2dig  = bin2dig' 0
bin2dig' :: Num t => t -> String -> t
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseFloat :: Parser LispVal
parseFloat = do
  sign <- many (oneOf "-+")
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  return $ Float $ negate sign  (fst . head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do
  sign <- many (oneOf "-+")
  x <- many1 digit
  _ <- char '/'
  y <- many1 digit
  return $ Ratio (negate sign (read x) % read y)

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n
toDouble _ = undefined

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseNumber
  _ <- char '+'
  _ <- many $ char '('
  y <- try parseFloat <|> parseNumber
  _ <- many $ char ')'
  _ <- char 'i'
  return $ Complex (toDouble x :+ toDouble y)

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    a <- endBy parseExpr spaces
    as <- char '.' >> spaces >> parseExpr
    return $ DottedList a as

parseLists :: Parser LispVal
parseLists = do
  _ <- char '('
  contents <- try parseList <|> parseDottedList
  _ <- char ')'
  return contents

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
  _ <- string ",@"
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseVectorInner :: Parser LispVal
parseVectorInner = do
  arrayValues <- sepBy parseExpr spaces
  return $ Vector (listArray (0, length arrayValues - 1) arrayValues)

parseVector :: Parser LispVal
parseVector = do
  _ <- string "#("
  x <- parseVectorInner
  _ <- char ')'
  return x

parseExpr :: Parser LispVal
parseExpr =
            try parseString
        <|> try parseRatio
        <|> try parseFloat
        <|> try parseComplex
        <|> try parseNumber
        <|> try parseAtom
        <|> try parseBool
        <|> try parseChar
        <|> try parseLists
        <|> try parseQuoted
        <|> try parseQuasiQuoted
        <|> try parseUnQuote
        <|> try parseUnQuoteSplicing
        <|> try parseVector
