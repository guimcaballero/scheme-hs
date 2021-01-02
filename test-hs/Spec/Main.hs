{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Types
import Parsing

-- import qualified Data.Text as T
import Data.Array
import Data.Ratio
import Data.Complex

import Test.Hspec
-- import System.IO.Unsafe


main :: IO ()
main = do
  hspec $ describe "src/Parser.hs" $ do

    it "Atom" $
      readExpr "bb-8?" `shouldBe` Right (Atom "bb-8?")

    it "Num Negative" $
      readExpr "-2187" `shouldBe` Right (Number (-2187))

    it "Num Positive" $
      readExpr "112233" `shouldBe` Right (Number 112233)

    it "Num Positive with Sign" $
      readExpr "+12345" `shouldBe` Right (Number 12345)

    it "Float Negative" $
      readExpr "-2187.1" `shouldBe` Right (Float (-2187.1))

    it "Float Positive" $
      readExpr "112233.1" `shouldBe` Right (Float 112233.1)

    it "Float Positive with Sign" $
      readExpr "+12345.3" `shouldBe` Right (Float 12345.3)

    it "Ratio Negative" $
      readExpr "-2187/3" `shouldBe` Right (Ratio (-2187 % 3))

    it "Ratio Positive" $
      readExpr "187/3" `shouldBe` Right (Ratio (187 % 3))

    it "Ratio Positive with Sign" $
      readExpr "+2187/3" `shouldBe` Right (Ratio (2187 % 3))

    it "Float Positive" $
      readExpr "112233.1" `shouldBe` Right (Float 112233.1)

    it "Float Positive with Sign" $
      readExpr "+12345.3" `shouldBe` Right (Float 12345.3)

    it "Complex Num with Sign" $
      readExpr "12345+3i" `shouldBe` Right (Complex (12345 :+ 3))

    it "Complex Num Positive with Sign" $
      readExpr "+12345+3i" `shouldBe` Right (Complex (12345 :+ 3))

    it "Complex Num Negative" $
      readExpr "-12345+3i" `shouldBe` Right (Complex ((-12345) :+ 3))

    it "Complex Num Negative" $
      readExpr "-12345+(-3)i" `shouldBe` Right (Complex ((-12345) :+ (-3)))

    it "String" $
      readExpr "\"Gen L Organa\"" `shouldBe` Right (String "Gen L Organa")

    it "Bool True" $
      readExpr "#t" `shouldBe` Right (Bool True)

    it "Bool False" $
      readExpr "#f" `shouldBe` Right (Bool False)

    it "S-Expr: homogenous list" $
      readExpr "(2 1 87)" `shouldBe`
      Right (List [Number 2, Number 1,Number 87])

    it "S-Expr: homogenous list quoted" $
      readExpr "'(2 1 87)" `shouldBe`
      Right (List [Atom "quote",List [Number 2, Number 1,Number 87]])

    it "S-Expr: heterogenous list" $
      readExpr "(stromTrooper \"Fn\" 2 1 87)" `shouldBe`
      Right (List [Atom "stromTrooper", String "Fn", Number 2, Number 1,Number 87])

    it "S-Expr: heterogenous list quoted" $
      readExpr "'(stromTrooper \"Fn\" 2 1 87)" `shouldBe`
      Right (List [Atom "quote", List [Atom "stromTrooper", String "Fn", Number 2, Number 1,Number 87]])

    it "S-Expr: vector" $
      readExpr "#(stromTrooper \"Fn\" 2 1 87)" `shouldBe`
      Right (Vector (listArray (0, 4) [Atom "stromTrooper", String "Fn", Number 2, Number 1,Number 87]))

    it "S-Expr: char space" $
      readExpr "(#\\space)" `shouldBe` Right (List [Char ' '])

    it "S-Expr: char newline" $
      readExpr "(#\\newline)" `shouldBe` Right (List [Char '\n'])

    it "S-Expr: char letter" $
      readExpr "(#\\y)" `shouldBe` Right (List [Char 'y'])

    it "S-Expr: single negative" $
      readExpr "(-42)" `shouldBe` Right (List [Number (-42)])

    it "S-Expr: (- num)" $
      readExpr "(- 42)" `shouldBe` Right (List [Atom "-", Number 42])

    it "S-Expr: prim call: numbers" $
      readExpr "(+ 1 2)" `shouldBe`
      Right (List [Atom "+", Number 1, Number 2])

    it "S-Expr: prim call: neg nums" $
      readExpr "(- -42 -42)" `shouldBe`
      Right (List [Atom "-", Number (-42), Number (-42)])

    it "S-Expr: prim call: atoms" $
      readExpr "(- rogue squadron)" `shouldBe`
      Right (List [Atom "-", Atom "rogue", Atom "squadron"])

    it "S-Expr: nested list" $
      readExpr "(lambda (x x) (+ x x))" `shouldBe`
      Right (List [Atom "lambda", List [Atom "x", Atom "x"], List [Atom "+", Atom "x", Atom "x"]])

    it "Comment: end-of/single line" $
      readExpr ";skip\nartoodetoo ;extra will throw\n;skip" `shouldBe` Right (Atom "artoodetoo")

    it "Comment: multi-line comment in single line" $
      readExpr "(c3 #|Hello how are you|# po)" `shouldBe` Right (List [Atom "c3",Atom "po"])

    it "Comment: multi-line line" $
      readExpr "#|Han\nShot\nFirst\n|#(c3 #| these are not the droids you're looking for|# po)\n #|Jar Jar Binks =?= Sith Lord |#" `shouldBe` Right (List [Atom "c3",Atom "po"])

  -- hspec $ describe "src/Eval.hs" $ do
  --   wStd "test/add.scm"              $ Number 3
  --   wStd "test/if_alt.scm"           $ Number 2
  --   wStd "test/let.scm"              $ Number 321
  --   wStd "test/eval_boolean.scm"     $ Bool True
  --   wStd "test/eval_boolean_ops.scm" $ Bool True
  --   wStd "test/eval_lambda.scm"      $ Number 5
  --   wStd "test/test_quote.scm"       $ List [Atom "xNotFound", Atom "yNotFound"]
  --   wStd "test/test_car.scm"         $ Number 1
  --   wStd "test/test_cdr.scm"         $ List [Number 2]
  --   wStd "test/test_cadadr.scm"      $ Number 42
  --   wStd "test/test_gt.scm"          $ List [ Bool True, Bool False]
  --   wStd "test/test_scope1.scm"      $ Number 413281
  --   wStd "test/test_scope2.scm"      $ Number 11
  --   wStd "test/test_args.scm"        $ Number 105065
  --   wStd "test/test_fold.scm"        $ Number 42
  --   runExpr Nothing "test/define.scm"        $ Number 4
  --   runExpr Nothing "test/define_order.scm"  $ Number 42
  --   runExpr Nothing "test/define_lambda.scm" $ String "smalltalk"
  --   runExpr Nothing "test/test_evalargs.scm" $ Number 1558
    --runExpr Nothing "test/test_fix.scm"      $ Number 3628800
    --runExpr Nothing "test/test_fix2.scm"      $ Number 5040




-- helper functions
-- run file w/ stdlib
-- wStd :: T.Text -> LispVal -> SpecWith ()
-- wStd = runExpr (Just "test/stdlib_mod.scm")

-- -- run expr w/o stdLib
-- tExpr :: T.Text -> T.Text -> LispVal -> SpecWith ()
-- tExpr note expr val =
--     it (T.unpack note) $ evalVal `shouldBe` val
--     where evalVal = (unsafePerformIO $ runASTinEnv basicEnv $ fileToEvalForm "" expr)


-- runExpr :: Maybe T.Text -> T.Text -> LispVal -> SpecWith ()
-- runExpr  std file val =
--     it (T.unpack file) $ evalVal  `shouldBe` val
--     where evalVal = unsafePerformIO $ evalTextTest std file

-- evalTextTest :: Maybe T.Text -> T.Text -> IO LispVal --REPL
-- evalTextTest (Just stdlib) file= do
--   stdlib <- getFileContents $ T.unpack  stdlib
--   f      <- getFileContents $ T.unpack file
--   runASTinEnv basicEnv $ textToEvalForm stdlib  f

-- evalTextTest Nothing file = do
--   f <- getFileContents $ T.unpack file
--   runASTinEnv basicEnv $ fileToEvalForm (T.unpack file) f

-- -- run text expr w/ file
-- tExprStd :: T.Text -> T.Text -> LispVal -> SpecWith ()
-- tExprStd note  expr  val =
--     it (T.unpack note ) $ evalVal `shouldBe` val
--     where evalVal = unsafePerformIO $ evalExprTest expr

-- evalExprTest ::  T.Text -> IO LispVal --REPL
-- evalExprTest expr = do
--   stdlib <- getFileContents $ T.unpack  "test/stdlib_mod.scm"
--   runASTinEnv basicEnv $ textToEvalForm stdlib  expr
