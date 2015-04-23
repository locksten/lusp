module Lusp.LispVal (LispVal(..)
                    ,Env(Env)) where

import Data.Complex (Complex, realPart, imagPart)
import Data.IORef (IORef)
import qualified Data.Map.Strict as Map (Map)
import Data.Ratio (numerator, denominator)
import System.IO (Handle)

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
          -- | Undefined return type
             | Void
             | PrimitiveFunc ([LispVal] -> LispVal)
             | IOFunc ([LispVal] -> IO LispVal)
          -- | User defined function
             | Func [String] (Maybe String) [LispVal] Env
             | Port Handle
             | EOF
instance Show LispVal where show = showLispVal

-- | Environment type containing the parent environment
-- and variables as name-value pairs
data Env = Env (Maybe Env, IORef (Map.Map String (IORef LispVal)))

showLispVal :: LispVal -> String
showLispVal (List [x])  = "(" ++ show x ++ ")"
showLispVal (List (x:xs)) = "(" ++ show x ++ concatMap ((" " ++) . show) xs
    ++ ")"
showLispVal (List [])   = "()"
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
showLispVal (IOFunc _)        = "<IO primitive>"
showLispVal (Func {})         = "<function>"
showLispVal (Port _)          = "<IO port>"
showLispVal (EOF)             = "<EOF>"
