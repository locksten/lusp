module Main (main) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertEqual
                  ,(@=?))

import Lusp.Parser (parse)
import Lusp.Evaluate (evaluate)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "List"
    [ testCase "cons" cons
    , testCase "car" car
    , testCase "cdr" cdr
    ]
  , testGroup "Numeric"
    [ testCase "non-decreasing" nonDecreasing
    , testCase "decreasing" decreasing
    , testCase "equal" equal
    , testCase "expt" expt
    ]
  , testGroup "Function definition"
    [ testCase "Function Definition 1" functionDefinition1
    , testCase "Function Definition 2" functionDefinition2
    ]
  , testGroup "Define"
    [ testCase "Define 1" define1
    , testCase "Define 2" define2
    ]
  , testGroup "Arithmetic"
    [ testCase "Addition" addition
    , testCase "Multiplication" multiplication
    , testCase "Subtraction" subtraction
    , testCase "Division" division
    ]
  , testGroup "Let binding"
    [ testCase "let 1" let1
    , testCase "let 2" let2
    , testCase "let 3" let3
    , testCase "let 4" let4
    ]
  ]

testLuspExpr expr result = (evaluate "" [] . parse) expr >>= \x ->
    result @=? showLast x
      where showLast xs = if null xs then "" else (show . last) xs

let1 = testLuspExpr "(let ((x 2) (y 3)) (* x y))"
    "6"
let2 = testLuspExpr "(let ((x 1) (y 20)) (let ((x 300) (z (+ x y))) (+ y z)))"
    "41"
let3 = testLuspExpr "(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))"
    "35"
let4 = testLuspExpr
    "(let ((count 0)) (lambda () (set! count (+ count 1)) count))\
    \(define foo-counter (let ((count 0)) (lambda () (set! count (+ count 1))\
    \count)))\
    \(foo-counter) (foo-counter) (foo-counter)"
    "3"

addition = testLuspExpr "(+ 3 30)"
    "33"
multiplication = testLuspExpr "(* 3 30)"
    "90"
subtraction = testLuspExpr "(- 3 30)"
    "-27"
division = testLuspExpr "(/ 3 30)"
    "1/10"

define1 = testLuspExpr "(define x (+ 3 30)) (- x 23)"
    "10"
define2 = testLuspExpr "(define x (+ 2 3)) (define y (* 2 5)) (+ x y)"
    "15"

functionDefinition1 = testLuspExpr "(define (square x) (* x x)) (square 4)"
    "16"
functionDefinition2 = testLuspExpr "(define (not x) (if x #f #t))\
    \(not (< 2 5))"
    "#f"

cons = testLuspExpr "(cons 'a '(b c))"
    "(a b c)"
car = testLuspExpr "(car '(a b c))"
    "a"
cdr = testLuspExpr "(cdr '(a b c))"
    "(b c)"

nonDecreasing = testLuspExpr "(<= 1 4 5 5 7 10 11)"
    "#t"
decreasing = testLuspExpr "(> 8 6 4 4 1)"
    "#f"
equal = testLuspExpr "(= 8 8 8 8 8)"
    "#t"
expt = testLuspExpr "(expt 2 4)"
    "16.0"
