module Lusp.Numeric where

import Lusp.LispError (LispError(NumArgs
                                ,TypeMismatch))
import Lusp.LispVal (LispVal(List
                            ,Integer
                            ,Float
                            ,Ratio))

import Control.Exception (throw)
import Data.Complex(Complex, realPart, imagPart)
import Data.Ratio(numerator, denominator)
