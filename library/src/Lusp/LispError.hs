module Lusp.LispError (LispError(..)) where

import Lusp.LispVal (LispVal)

import Text.Parsec.Error (ParseError)

import Control.Exception (Exception)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | ParseError ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | DivBy0
               | Other String
instance Show LispError where show = showError
instance Exception LispError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                        ++ " args; found values " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                        ++ ", found " ++ show found
showError (ParseError parseErr)         = "Parse error at " ++ show parseErr
showError (DivBy0)                      = "Attempted division by 0"
showError (Other str)                   = show str
