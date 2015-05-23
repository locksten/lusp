module Lusp.LispValUtils (prettyPrint
                         ,isVoid
                         ,isEOF
                         ,isSymbol
                         ,isInteger
                         ,isReal
                         ,isRatio
                         ,isComplex
                         ,isPrimitiveFunc
                         ,isIOFunc
                         ,isFunc
                         ,isPort
                         ,extractList
                         ,extractChar
                         ,extractInt
                         ,extractStr) where

import Lusp.LispError (LispError(ParseError
                                ,TypeMismatch))
import Lusp.LispVal (LispVal(List
                            ,String
                            ,Char
                            ,Void
                            ,EOF
                            ,Integer
                            ,Real
                            ,Ratio
                            ,Complex
                            ,Atom
                            ,PrimitiveFunc
                            ,IOFunc
                            ,Func
                            ,Port))
import Lusp.Parser (parseString)

import Control.Exception (throw)
import Text.ParserCombinators.Parsec (parse)

prettyPrint :: LispVal -> String
prettyPrint (String x) = case parse parseString "" ("\"" ++ x ++ "\"") of
                           Left e             -> throw $ ParseError e
                           Right (String val) -> val
                           Right _            -> error "Expected String"
prettyPrint (Char x) = [x]
prettyPrint x = show x

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _        = False

isVoid :: LispVal -> Bool
isVoid Void = True
isVoid _    = False

isFunc :: LispVal -> Bool
isFunc (Func {}) = True
isFunc _         = False

isIOFunc :: LispVal -> Bool
isIOFunc (IOFunc _) = True
isIOFunc _          = False

isPrimitiveFunc :: LispVal -> Bool
isPrimitiveFunc (PrimitiveFunc _) = True
isPrimitiveFunc _                 = False

isPort :: LispVal -> Bool
isPort (Port _) = True
isPort _        = False

isEOF :: LispVal -> Bool
isEOF EOF = True
isEOF _   = False

isInteger :: LispVal -> Bool
isInteger (Integer _) = True
isInteger _           = False

isReal :: LispVal -> Bool
isReal (Real _)       = True
isReal _              = False

isRatio :: LispVal -> Bool
isRatio (Ratio _)     = True
isRatio _             = False

isComplex :: LispVal -> Bool
isComplex (Complex _) = True
isComplex _           = False

extractList :: LispVal -> [LispVal]
extractList (List x) = x
extractList x        = throw $ TypeMismatch "List" x

extractChar :: LispVal -> Char
extractChar (Char x) = x
extractChar x        = throw $ TypeMismatch "Char" x

extractInt :: LispVal -> Integer
extractInt (Integer x) = x
extractInt x           = throw $ TypeMismatch "Integer" x

extractStr :: LispVal -> String
extractStr (String x) = x
extractStr x          = throw $ TypeMismatch "String" x
