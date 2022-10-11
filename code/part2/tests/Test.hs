-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit


testParseString = testGroup "Unit Tests for the 'parseString' function"
  [
    testCase "Constant: 3" $
    ((parseString "3" :: Either ParseError Program) @?= 
    (Right [SExp (Const (IntVal 3))] :: Either ParseError Program)),

    testCase "Negative constant: -5" $
    ((parseString "-5" :: Either ParseError Program) @?= 
    (Right [SExp (Const (IntVal (-5)))] :: Either ParseError Program)),

    testCase "Addition: 5+1" $
    ((parseString "5+1" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Plus (Const (IntVal 5)) (Const (IntVal 1)))] :: Either ParseError Program)),

    testCase "Substraction: 9-5" $
    ((parseString "9-5" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Minus (Const (IntVal 9)) (Const (IntVal 5)))] :: Either ParseError Program)),

    testCase "Multiplication: 2*3" $
    ((parseString "2*3" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Times (Const (IntVal 2)) (Const (IntVal 3)))] :: Either ParseError Program)),

    testCase "Division: 5//5" $
    ((parseString "5//5" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Div (Const (IntVal 5)) (Const (IntVal 5)))] :: Either ParseError Program)),

    testCase "Modulo: 3%2" $
    ((parseString "3%2" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Mod (Const (IntVal 3)) (Const (IntVal 2)))] :: Either ParseError Program)),

    testCase "In: 3 in [] " $
    ((parseString "3 in []" :: Either ParseError Program) @?= 
    (Right [SExp (Oper In (Const (IntVal 3)) (List []))] :: Either ParseError Program)),

    testCase "Defining variable: a=5" $
    ((parseString "a=5" :: Either ParseError Program) @?= 
    (Right [SDef "a" (Const (IntVal 5))] :: Either ParseError Program)),

     testCase "Multiple statements: a=5; a*7" $
    ((parseString "a=5; a*7" :: Either ParseError Program) @?= 
    (Right [SDef "a" (Const (IntVal 5)),SExp (Oper Times (Var "a") (Const (IntVal 7)))] 
    :: Either ParseError Program)),

    testCase "Checking for equality: 5 == 5" $
    ((parseString "5 == 5" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Eq (Const (IntVal 5)) (Const (IntVal 5)))]
    :: Either ParseError Program)),   

    testCase "Checking for inequality: 2 != 2" $
    ((parseString "2 != 2" :: Either ParseError Program) @?= 
    (Right [SExp (Not (Oper Eq (Const (IntVal 2)) (Const (IntVal 2))))]
    :: Either ParseError Program)),

    testCase "Parsing None value: None" $
    ((parseString "None" :: Either ParseError Program) @?= 
    (Right [SExp (Const NoneVal)]
    :: Either ParseError Program)),

    testCase "Parsing True value: True" $
    ((parseString "True" :: Either ParseError Program) @?= 
    (Right [SExp (Const TrueVal)]
    :: Either ParseError Program)),

    testCase "Parsing False value: False" $
    ((parseString "False" :: Either ParseError Program) @?= 
    (Right [SExp (Const FalseVal)]
    :: Either ParseError Program)),

    testCase "Parsing List: [3,4]" $
    ((parseString "[3,4]" :: Either ParseError Program) @?= 
    (Right [SExp (List [Const (IntVal 3),Const (IntVal 4)])]
    :: Either ParseError Program)),

    testCase "Parsing List of integers: [3,4]" $
    ((parseString "[3,4]" :: Either ParseError Program) @?= 
    (Right [SExp (List [Const (IntVal 3),Const (IntVal 4)])]
    :: Either ParseError Program)),

    testCase "Parsing List of values: [3,4]" $
    ((parseString "[3,4]" :: Either ParseError Program) @?= 
    (Right [SExp (List [Const (IntVal 3),Const (IntVal 4)])]
    :: Either ParseError Program)),

    testCase "Parsing empty List: []" $
    ((parseString "[]" :: Either ParseError Program) @?= 
    (Right [SExp (List [])]
    :: Either ParseError Program)),

    testCase "Parsing not: not 5" $
    ((parseString "not 5" :: Either ParseError Program) @?= 
    (Right [SExp (Not (Const (IntVal 5)))]
    :: Either ParseError Program)),

    testCase "Parsing greater than: 5>6" $
    ((parseString "5>6" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Greater (Const (IntVal 5)) (Const (IntVal 6)))]
    :: Either ParseError Program)),

    testCase "Parsing less than: 5<6" $
    ((parseString "5<6" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Less(Const (IntVal 5)) (Const (IntVal 6)))]
    :: Either ParseError Program)),

    testCase "Parsing if-statement: if" $
    ((parseString "[x for x in 2+2 if 3>2]" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Less(Const (IntVal 5)) (Const (IntVal 6)))]
    :: Either ParseError Program)),

    testCase "Parsing function calls: print(3)" $
    ((parseString "print(3)" :: Either ParseError Program) @?= 
    (Right [SExp (Call "print" [Const (IntVal 3)])]
    :: Either ParseError Program)),

    testCase "Parsing list comprehensions: [x for x in 2+2]" $
    ((parseString "[x for x in 2+2]" :: Either ParseError Program) @?= 
    (Right [SExp (Compr (Var "x") [CCFor "x" (Oper Plus (Const (IntVal 2)) (Const (IntVal 2)))])]
    :: Either ParseError Program)),

    testCase "Parsing string: 'hello world!'" $
    ((parseString "'hello world!'" :: Either ParseError Program) @?= 
    (Right [SExp (Const (StringVal "hello world!"))]
    :: Either ParseError Program)),

    testCase "Defining false name: 2a = 4" $
    ((parseString "2a = 4" :: Either ParseError Program) @?= 
    (Left "Parsing Error"
    :: Either ParseError Program)),

    testCase "Defining name with _: _ = 3" $
    ((parseString "_ = 3" :: Either ParseError Program) @?= 
    (Right [SDef "_" (Const (IntVal 3))]
    :: Either ParseError Program)),

    testCase "Defining name with number in: _2 = 3" $
    ((parseString "_2 = 3" :: Either ParseError Program) @?= 
    (Right [SDef "_2" (Const (IntVal 3))]
    :: Either ParseError Program)),

    testCase "Parsing wrong grama: 3 * + 5" $
    ((parseString "3 * + 5" :: Either ParseError Program) @?= 
    (Left "Parsing Error"
    :: Either ParseError Program)),

    testCase "Parsing wrong grama: 3 - + 5" $
    ((parseString "3 * + 5" :: Either ParseError Program) @?= 
    (Left "Parsing Error"
    :: Either ParseError Program)),

    testCase "Parsing wrong grama: 3 * > 5" $
    ((parseString "3 * > 5" :: Either ParseError Program) @?= 
    (Left "Parsing Error"
    :: Either ParseError Program)),

    testCase "Parsing expression inside parenthesis: 3 - + 5" $
    ((parseString "(2+3)" :: Either ParseError Program) @?= 
    (Right [SExp (Oper Plus (Const (IntVal 2)) (Const (IntVal 3)))]
    :: Either ParseError Program))
  ]


tests = testGroup "Tests" [testParseString]

main :: IO ()
main = defaultMain tests

-- main :: IO ()
-- main = defaultMain $ localOption (mkTimeout 1000000) tests

-- tests = testGroup "Minimal tests" [
--   testCase "simple success" $
--     parseString "2 + two" @?=
--       Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
--   testCase "simple failure" $
--     -- avoid "expecting" very specific parse-error messages
--     case parseString "wow!" of
--       Left e -> return ()  -- any message is OK
--       Right p -> assertFailure $ "Unexpected parse: " ++ show p]
