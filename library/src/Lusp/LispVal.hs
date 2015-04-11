module Lusp.LispVal (LispVal(..)
                    ,isVoid
                    ,Env) where

import Data.Complex (Complex, realPart, imagPart)
import Data.IORef (IORef)
import Data.Ratio (numerator, denominator)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector [LispVal]
             | Integer Integer
             | Real Double
             | Ratio Rational
             | Complex (Complex Double)
             | String String
             | Char Char
             | Bool Bool
             | Void
             | PrimitiveFunc ([LispVal] -> LispVal)
             | Func [String] (Maybe String) [LispVal] Env
instance Show LispVal where show = showLispVal

type Env = IORef [(String, IORef LispVal)]

showLispVal :: LispVal -> String
showLispVal (List xs)   =  concatMap ((++ " ") . show) xs
showLispVal (Vector xs) = "#" ++ show (List xs)
showLispVal (DottedList xs end) = "(" ++ (tail . init . show) (List xs) ++
    " . " ++ show end ++ ")"
showLispVal (Atom x)    = x
showLispVal (Integer x) = show x
showLispVal (Real x)    = show x
showLispVal (Ratio x)   = show (numerator x) ++ "/" ++ show (denominator x)
showLispVal (Complex x) = show (realPart x) ++ "+" ++ show (imagPart x) ++ "i"
showLispVal (String x)  = show x
showLispVal (Char x)    = show x
showLispVal (Bool x)    = if x then "#t" else "#f"
showLispVal (Void)      = "#void"
showLispVal (PrimitiveFunc _) = "<primitive>"
showLispVal (Func _ _ _ _)    = "<function>"

isVoid :: LispVal -> Bool
isVoid Void = True
isVoid _ = False
