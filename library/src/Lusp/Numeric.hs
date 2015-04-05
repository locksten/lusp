module Lusp.Numeric where

import Lusp.LispError (LispError(NumArgs
                                ,TypeMismatch
                                ,Other))

import Lusp.LispVal (LispVal(Integer
                            ,Float
                            ,Ratio
                            ,Complex))

import Control.Exception (throw)
import Data.Complex (Complex((:+)))
import Data.Ratio (numerator, denominator)

numCast :: LispVal -> LispVal -> (LispVal, LispVal)
numCast a@(Integer _) b@(Integer _) = (a, b)
numCast a@(Float   _) b@(Float   _) = (a, b)
numCast a@(Ratio   _) b@(Ratio   _) = (a, b)
numCast a@(Complex _) b@(Complex _) = (a, b)
numCast   (Integer a) b@(Float   _) = (  Float    $ fromInteger a, b)
numCast   (Integer a) b@(Ratio   _) = (  Ratio    $ fromInteger a, b)
numCast   (Integer a) b@(Complex _) = (  Complex  $ fromInteger a, b)
numCast a@(Float _)     (Integer b) = (a, Float   $ fromInteger b)
numCast a@(Float _)     (Ratio   b) = (a, Float   $ fromRational b)
numCast   (Float a)   b@(Complex _) = (   Complex $ a :+ 0, b)
numCast a@(Ratio _)     (Integer b) = (a, Ratio   $ fromInteger b)
numCast   (Ratio a)   b@(Float   _) = (   Float   $ fromRational a, b)
numCast c@(Ratio _)   b@(Complex _) = (   ratioToComplex c, b)
numCast a@(Complex _)   (Integer b) = (a, Complex $ fromInteger b)
numCast a@(Complex _)   (Float   b) = (a, Complex $ b :+ 0)
numCast a@(Complex _) c@(Ratio   _) = (a, ratioToComplex c)
numCast a b = case a of
                Integer _  -> err b
                Float _    -> err b
                Ratio _    -> err b
                Complex _  -> err b
                _          -> err a
  where err notNum = throw $ TypeMismatch "number" notNum

ratioToComplex :: LispVal -> LispVal
ratioToComplex (Ratio x) = Complex $ (fromInteger $ numerator x)
                                   / (fromInteger $ denominator x)
ratioToComplex _ = error "Expected Ratio"
