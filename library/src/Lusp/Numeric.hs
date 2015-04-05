module Lusp.Numeric (add
                    ,subtract
                    ,multiply
                    ,divide
                    ,modulo
                    ,remainder) where

import Lusp.LispError (LispError(NumArgs
                                ,TypeMismatch
                                ,DivBy0))

import Lusp.LispVal (LispVal(Integer
                            ,Ratio
                            ,Float
                            ,Complex))

import Control.Exception (throw)
import Data.Complex (Complex((:+)))
import Data.Ratio ((%), numerator, denominator)

import Prelude hiding (div
                      ,subtract)
import qualified Prelude as P (div)

numCast :: LispVal -> LispVal -> (LispVal, LispVal)
numCast a@(Integer _) b@(Integer _) = (a, b)
numCast a@(Ratio   _) b@(Ratio   _) = (a, b)
numCast a@(Float   _) b@(Float   _) = (a, b)
numCast a@(Complex _) b@(Complex _) = (a, b)
numCast   (Integer a) b@(Float   _) = (  Float    $ fromInteger a, b)
numCast   (Integer a) b@(Ratio   _) = (  Ratio    $ fromInteger a, b)
numCast   (Integer a) b@(Complex _) = (  Complex  $ fromInteger a, b)
numCast a@(Ratio _)     (Integer b) = (a, Ratio   $ fromInteger b)
numCast   (Ratio a)   b@(Float   _) = (   Float   $ fromRational a, b)
numCast c@(Ratio _)   b@(Complex _) = (   ratioToComplex c, b)
numCast a@(Float _)     (Integer b) = (a, Float   $ fromInteger b)
numCast a@(Float _)     (Ratio   b) = (a, Float   $ fromRational b)
numCast   (Float a)   b@(Complex _) = (   Complex $ a :+ 0, b)
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

add :: [LispVal] -> LispVal
add [] = Integer 0
add params = foldl1 (\x y -> add' $ numCast x y) params
  where add' ((Integer a), (Integer b)) = Integer $ a + b
        add' ((Ratio   a), (Ratio   b)) = Ratio   $ a + b
        add' ((Float   a), (Float   b)) = Float   $ a + b
        add' ((Complex a), (Complex b)) = Complex $ a + b
        add' _ = error "Expected Number"

subtract :: [LispVal] -> LispVal
subtract [] = throw $ NumArgs 1 []
subtract [Integer x] = Integer $ -x
subtract [Ratio   x] = Ratio   $ -x
subtract [Float   x] = Float   $ -x
subtract [Complex x] = Complex $ -x
subtract params = foldl1 (\x y -> sub $ numCast x y) params
  where sub ((Integer a), (Integer b)) = Integer $ a - b
        sub ((Ratio   a), (Ratio   b)) = Ratio   $ a - b
        sub ((Float   a), (Float   b)) = Float   $ a - b
        sub ((Complex a), (Complex b)) = Complex $ a - b
        sub _ = error "Expected Number"

multiply :: [LispVal] -> LispVal
multiply [] = Integer 1
multiply params = foldl1 (\x y -> mul $ numCast x y) params
  where mul ((Integer a), (Integer b)) = Integer $ a * b
        mul ((Ratio   a), (Ratio   b)) = Ratio   $ a * b
        mul ((Float   a), (Float   b)) = Float   $ a * b
        mul ((Complex a), (Complex b)) = Complex $ a * b
        mul _ = error "Expected Number"

divide :: [LispVal] -> LispVal
divide [] = throw $ NumArgs 1 []
divide [Integer x] = divide [Integer 1, Integer x]
divide [Ratio   x] = divide [Integer 1, Ratio   x]
divide [Float   x] = divide [Integer 1, Float   x]
divide [Complex x] = divide [Integer 1, Complex x]
divide params = foldl1 (\x y -> div $ numCast x y) params
  where div ((Integer a), (Integer b))
            | b           == 0 = err
            | (a `mod` b) == 0 = Integer (a `P.div` b)
            | otherwise        = Ratio (a % b)
        div ((Ratio a), (Ratio b))
            | b == 0    = err
            | otherwise = Ratio (a / b)
        div ((Float a), (Float b))
            | b == 0    = err
            | otherwise = Float (a / b)
        div ((Complex a), (Complex b))
            | b == 0    = err
            | otherwise = Complex (a / b)
        div _ = error "Expected Number"
        err = throw DivBy0

modulo :: [LispVal] -> LispVal
modulo [(Integer _), (Integer 0)] = throw DivBy0
modulo [(Integer a), (Integer b)] = Integer (a `mod` b)
modulo [(Integer _), x] = throw $ TypeMismatch "integer" x
modulo [x, (Integer _)] = throw $ TypeMismatch "integer" x
modulo xs = throw $ NumArgs 2 xs

remainder :: [LispVal] -> LispVal
remainder [(Integer _), (Integer 0)] = throw DivBy0
remainder [(Integer a), (Integer b)] = Integer (a `rem` b)
remainder [(Integer _), x] = throw $ TypeMismatch "integer" x
remainder [x, (Integer _)] = throw $ TypeMismatch "integer" x
remainder xs = throw $ NumArgs 2 xs
