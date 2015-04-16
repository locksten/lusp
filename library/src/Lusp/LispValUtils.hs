module Lusp.LispValUtils (prettyPrint
                         ,isVoid
                         ,isEOF
                         ,extractList) where

import Lusp.LispError (LispError(ParseError
                                ,TypeMismatch))
import Lusp.LispVal (LispVal(List
                            ,String
                            ,Char
                            ,Void
                            ,EOF))
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

isVoid :: LispVal -> Bool
isVoid Void = True
isVoid _ = False

isEOF :: LispVal -> Bool
isEOF EOF = True
isEOF _   = False

extractList :: LispVal -> [LispVal]
extractList (List x) = x
extractList x        = throw $ TypeMismatch "List" x
