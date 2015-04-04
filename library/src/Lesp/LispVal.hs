module Lesp.LispVal (LispVal(..)) where

import Data.Complex
import Data.Ratio

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector [LispVal]
             | Integer Integer
             | Float Float
             | Ratio Rational
             | Complex (Complex Float)
             | String String
             | Char Char
             | Bool Bool
instance Show LispVal where show = prettyPrint

prettyPrintList :: (LispVal, Int) -> String
prettyPrintList (xs, depth) = case xs of
    (List ys) -> indent depth ++ "( " ++ concatMap iteration ys ++ ") "
      where iteration ts = prettyPrintList (ts, depth + 1)
            indent d = if d == 0 then "" else "\n" ++ concat
                (replicate (d * prettyPrintIndentLevel) " ")
            prettyPrintIndentLevel :: Int
            prettyPrintIndentLevel = 4
    _         -> prettyPrint xs

prettyPrint :: LispVal -> String
prettyPrint (List xs) = prettyPrintList (List xs, 0)
prettyPrint (Vector xs) = "#" ++ prettyPrintList (List xs, 0)
prettyPrint (DottedList xs end) = "( " ++ concatMap prettyPrint xs ++ ". " ++
                                  prettyPrint end ++ ") "
prettyPrint (Atom x) = (init $ tail $ show x) ++ " "
prettyPrint (Integer x) = show x ++ " "
prettyPrint (Float x) = show x ++ " "
prettyPrint (Ratio x) = show (numerator x) ++ "/" ++ show (denominator x)
                        ++ " "
prettyPrint (Complex x) = show (realPart x) ++ "+" ++ show (imagPart x) ++ " "
prettyPrint (String x) = show x ++ " "
prettyPrint (Char x) = "#\\" ++ init (tail $ show x) ++ " "
prettyPrint (Bool x) = if x then "#t " else "#f "
