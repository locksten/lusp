module Lusp.Eval (eval) where

import Lusp.LispError (LispError(..))
import Lusp.LispVal (LispVal(..))

import Control.Exception (throw)

eval :: LispVal -> LispVal
eval v@(String _)  = v
eval v@(Integer _) = v
eval v@(Float _)   = v
eval v@(Ratio _)   = v
eval v@(Complex _) = v
eval v@(Bool _)    = v
eval v@(Char _)    = v
eval (List [Atom "quote", v]) = v
eval badForm = throw $ BadSpecialForm "Unrecognized special form" badForm
