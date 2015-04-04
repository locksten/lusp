module Lesp.Eval (eval) where

import Lesp.LispVal

eval :: LispVal -> LispVal
eval v@(String _)  = v
eval v@(Integer _) = v
eval v@(Float _)   = v
eval v@(Ratio _)   = v
eval v@(Complex _) = v
eval v@(Bool _)    = v
eval v@(Char _)    = v
eval (List [Atom "quote", v]) = v
