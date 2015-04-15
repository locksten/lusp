module Lusp.Numeric (add
                    ,subtract
                    ,multiply
                    ,divide
                    ,modulo
                    ,remainder
                    ,quotient) where

import Lusp.LispError (LispError(NumArgs
                                ,TypeMismatch
                                ,DivBy0))
import Lusp.LispVal (LispVal(Integer
                            ,Ratio
                            ,Real
                            ,Complex))

import Prelude hiding (div
                      ,subtract)
import qualified Prelude as P (div)

import Control.Exception (throw)
import Data.Complex (Complex((:+)))
import Data.Ratio ((%), numerator, denominator)

numCast :: LispVal -> LispVal -> (LispVal, LispVal)
numCast a@(Integer _) b@(Integer _) = (a, b)
numCast a@(Ratio   _) b@(Ratio   _) = (a, b)
numCast a@(Real    _) b@(Real    _) = (a, b)
numCast a@(Complex _) b@(Complex _) = (a, b)
numCast   (Integer a) b@(Real    _) = (   Real    $ fromInteger a, b)
numCast   (Integer a) b@(Ratio   _) = (   Ratio   $ fromInteger a, b)
numCast   (Integer a) b@(Complex _) = (   Complex $ fromInteger a, b)
numCast a@(Ratio   _)   (Integer b) = (a, Ratio   $ fromInteger b)
numCast   (Ratio   a) b@(Real    _) = (   Real    $ fromRational a, b)
numCast c@(Ratio   _) b@(Complex _) = (   ratioToComplex c, b)
numCast a@(Real    _)   (Integer b) = (a, Real    $ fromInteger b)
numCast a@(Real    _)   (Ratio   b) = (a, Real    $ fromRational b)
numCast   (Real    a) b@(Complex _) = (   Complex $ a :+ 0, b)
numCast a@(Complex _)   (Integer b) = (a, Complex $ fromInteger b)
numCast a@(Complex _)   (Real    b) = (a, Complex $ b :+ 0)
numCast a@(Complex _) c@(Ratio   _) = (a, ratioToComplex c)
numCast a b = case a of
                Integer _  -> err b
                Real _     -> err b
                Ratio _    -> err b
                Complex _  -> err b
                _          -> err a
  where err notNum = throw $ TypeMismatch "number" notNum

ratioToComplex :: LispVal -> LispVal
ratioToComplex (Ratio x) = Complex $ fromInteger (numerator x)
                                   / fromInteger (denominator x)
ratioToComplex _ = error "Expected Ratio"

add :: [LispVal] -> LispVal
add [] = Integer 0
add params = foldl1 (\x y -> add' $ numCast x y) params
  where add' (Integer a, Integer b) = Integer $ a + b
        add' (Ratio   a, Ratio   b) = Ratio   $ a + b
        add' (Real    a, Real    b) = Real    $ a + b
        add' (Complex a, Complex b) = Complex $ a + b
        add' _ = error "Expected Number"

subtract :: [LispVal] -> LispVal
subtract [] = throw $ NumArgs "1" []
subtract [Integer x] = Integer $ -x
subtract [Ratio   x] = Ratio   $ -x
subtract [Real    x] = Real    $ -x
subtract [Complex x] = Complex $ -x
subtract params = foldl1 (\x y -> sub $ numCast x y) params
  where sub (Integer a, Integer b) = Integer $ a - b
        sub (Ratio   a, Ratio   b) = Ratio   $ a - b
        sub (Real    a, Real    b) = Real    $ a - b
        sub (Complex a, Complex b) = Complex $ a - b
        sub _ = error "Expected Number"

multiply :: [LispVal] -> LispVal
multiply [] = Integer 1
multiply params = foldl1 (\x y -> mul $ numCast x y) params
  where mul (Integer a, Integer b) = Integer $ a * b
        mul (Ratio   a, Ratio   b) = Ratio   $ a * b
        mul (Real    a, Real    b) = Real    $ a * b
        mul (Complex a, Complex b) = Complex $ a * b
        mul _ = error "Expected Number"

divide :: [LispVal] -> LispVal
divide [] = throw $ NumArgs "1" []
divide [Integer x] = divide [Integer 1, Integer x]
divide [Ratio   x] = divide [Integer 1, Ratio   x]
divide [Real    x] = divide [Integer 1, Real    x]
divide [Complex x] = divide [Integer 1, Complex x]
divide params = foldl1 (\x y -> div $ numCast x y) params
  where div (Integer a, Integer b)
            | b           == 0 = err
            | (a `mod` b) == 0 = Integer (a `P.div` b)
            | otherwise        = Ratio (a % b)
        div (Ratio a, Ratio b)
            | b == 0    = err
            | otherwise = Ratio (a / b)
        div (Real a, Real b)
            | b == 0    = err
            | otherwise = Real (a / b)
        div (Complex a, Complex b)
            | b == 0    = err
            | otherwise = Complex (a / b)
        div _ = error "Expected Number"
        err = throw DivBy0

modulo :: [LispVal] -> LispVal
modulo xs = integerBinDivOp xs mod

remainder :: [LispVal] -> LispVal
remainder xs = integerBinDivOp xs rem

quotient :: [LispVal] -> LispVal
quotient xs = integerBinDivOp xs quot

integerBinDivOp :: [LispVal] -> (Integer -> Integer -> Integer) -> LispVal
integerBinDivOp [Integer _, Integer 0] _  = throw DivBy0
integerBinDivOp [Integer a, Integer b] op = Integer (a `op` b)
integerBinDivOp [Integer _, x] _          = throw $ TypeMismatch "integer" x
integerBinDivOp [x, Integer _] _        = throw $ TypeMismatch "integer" x
integerBinDivOp xs _                      = throw $ NumArgs "2" xs
