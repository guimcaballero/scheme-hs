{-# Language RankNTypes #-}

module Parsing where

import Prelude hiding (negate)

-- Inspiration from
-- https://github.com/justinethier/husk-scheme/blob/master/hs-src/Language/Scheme/Parser.hs

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Language
import qualified Text.Parsec.Token as P
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT)

import qualified Data.Char as DC
import Numeric
import Data.Complex
import Data.Ratio
import Data.Array
import Control.Monad.Except

import qualified Data.Text as T

import Types

style :: LanguageDef ()
style =
  emptyDef
    { P.commentStart = "#|"
    , P.commentEnd = "|#"
    , P.commentLine = ";"
    , P.nestedComments = True
    , P.identStart = letter <|> symbol
    , P.identLetter = letter <|> digit <|> symbol
    , P.reservedNames = []
    , P.caseSensitive = True
    }

lexer :: P.GenTokenParser String () Data.Functor.Identity.Identity
lexer = P.makeTokenParser style

dot :: ParsecT String () Identity String
dot = P.dot lexer

parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = P.parens lexer

braces :: ParsecT String () Identity a -> ParsecT String () Identity a
braces = P.braces lexer

brackets :: ParsecT String () Identity a -> ParsecT String () Identity a
brackets = P.brackets lexer

identifier :: ParsecT String () Identity String
identifier = P.identifier lexer

whiteSpace :: ParsecT String () Identity ()
whiteSpace = P.whiteSpace lexer

lexeme :: ParsecT String () Identity a -> ParsecT String () Identity a
lexeme = P.lexeme lexer

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~."

-- |Parse an atom (scheme symbol)
parseAtom :: Parser LispVal
parseAtom = do
  atom <- identifier
  if atom == "."
     then pzero -- Do not match this form
     else return $ Atom $ T.pack atom

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseEscapedChar :: forall st .
                    GenParser Char st Char
parseEscapedChar = do
  _ <- char '\\'
  c <- anyChar
  case c of
    'a' -> return '\a'
    'b' -> return '\b'
    'n' -> return '\n'
    't' -> return '\t'
    'r' -> return '\r'
    'x' -> do
        num <- many $ letter <|> digit
        _ <- char ';'
        parseHexScalar num
    _ -> return c

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (parseEscapedChar <|> noneOf "\"")
  _ <- char '"'
  return $ String $ T.pack x

parseChar :: Parser LispVal
parseChar = do
  _ <- try (string "#\\")
  c <- anyChar
  r <- many (letter <|> digit)
  let pchr = c : r
  case pchr of
    "space"     -> return $ Char ' '
    "newline"   -> return $ Char '\n'
    "alarm"     -> return $ Char '\a'
    "backspace" -> return $ Char '\b'
    "delete"    -> return $ Char '\DEL'
    "escape"    -> return $ Char '\ESC'
    "null"      -> return $ Char '\0'
    "return"    -> return $ Char '\n'
    "tab"       -> return $ Char '\t'
    _ -> case (c : r) of
        [ch] -> return $ Char ch
        ('x' : hexs) -> do
            rv <- parseHexScalar hexs
            return $ Char rv
        _ -> pzero

parseHexScalar :: String -> GenParser Char st Char
parseHexScalar num = do
    let ns = Numeric.readHex num
    case ns of
        [] -> fail $ "Unable to parse hex value " ++ show num
        _ -> return $ DC.chr $ fst $ head ns

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
parseList = List <$> sepBy parseExpr whiteSpace

parseDottedList :: Parser LispVal
parseDottedList = do
  phead <- endBy parseExpr whiteSpace
  case phead of
    [] -> pzero -- car is required; no match
    _ -> do
      ptail <- dot >> parseExpr
      case ptail of
        DottedList ls l -> return $ DottedList (phead ++ ls) l
        -- Issue #41
        -- Improper lists are tricky because if an improper list ends in a
        -- proper list, then it becomes proper as well. The following cases
        -- handle that, as well as preserving necessary functionality when
        -- appropriate, such as for unquoting.
        --
        -- FUTURE: I am not sure if this is complete, in fact the "unquote"
        -- seems like it could either be incorrect or one special case among
        -- others. Anyway, for the 3.3 release this is good enough to pass all
        -- test cases. It will be revisited later if necessary.
        --
        List (Atom "unquote" : _) -> return $ DottedList phead ptail
        List ls -> return $ List $ phead ++ ls
        {- Regarding above, see
           http://community.schemewiki.org/?scheme-faq-language#dottedapp

           Note, however, that most Schemes expand literal lists occurring in
           function applications, e.g. (foo bar . (1 2 3)) is expanded into
           (foo bar 1 2 3) by the reader. It is not entirely clear whether this
           is a consequence of the standard - the notation is not part of the
           R5RS grammar but there is strong evidence to suggest a Scheme
           implementation cannot comply with all of R5RS without performing this
           transformation. -}
        _ -> return $ DottedList phead ptail

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- lexeme $ char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- lexeme $ char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  _ <- try (lexeme $ char ',')
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSpliced :: Parser LispVal
parseUnquoteSpliced = do
  _ <- try (lexeme $ string ",@")
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

-- |Parse a vector
parseVector :: Parser LispVal
parseVector = do
  vals <- sepBy parseExpr whiteSpace
  return $ Vector (listArray (0, length vals - 1) vals)

parseExpr :: Parser LispVal
parseExpr =
      try (lexeme parseComplex)
  <|> try (lexeme parseRatio)
  <|> try (lexeme parseFloat)
  <|> try (lexeme parseNumber)
  <|> lexeme parseChar
  <|> parseUnquoteSpliced
  <|> do _ <- try (lexeme $ string "#(")
         x <- parseVector
         _ <- lexeme $ char ')'
         return x
  -- <|> do _ <- try (lexeme $ string "#u8(")
  --        x <- parseByteVector
  --        _ <- lexeme $ char ')'
  --        return x
--  <|> do _ <- try (lexeme $ string "#hash(")
--         x <- parseHashTable
--         _ <- lexeme $ char ')'
--         return x
  <|> try parseAtom
  <|> lexeme parseString
  <|> lexeme parseBool
  <|> parseQuoted
  <|> parseQuasiQuoted
  <|> parseUnquoted
  <|> try (parens parseList)
  <|> parens parseDottedList
  <|> try (brackets parseList)
  <|> brackets parseDottedList
  <|> try (braces parseList)
  <|> braces parseDottedList
  <?> "Expression"

-- |Initial parser used by the high-level parse functions
mainParser :: Parser LispVal
mainParser = do
    _ <- whiteSpace
    parseExpr

-- |Use a parser to parse the given text, throwing an error
--  if there is a problem parsing the text.
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-- |Parse an expression from a string of text
readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow mainParser

-- |Parse many expressions from a string of text
readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy mainParser whiteSpace)
