module Lusp.LispValUtils (prettyPrint
                         ,isVoid
                         ,isEOF) where

import Lusp.LispError (LispError(ParseError))
import Lusp.LispVal (LispVal(String
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
