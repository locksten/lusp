module Lusp.Numeric (equalElems
                    ,increasing
                    ,decreasing
                    ,nonIncreasing
                    ,nonDecreasing
                    ,sqrt
                    ,log
                    ,exp
                    ,floor
                    ,ceiling
                    ,truncate
                    ,round
                    ,expt
                    ,add
                    ,subtract
                    ,multiply
                    ,divide
                    ,modulo
                    ,remainder
                    ,quotient
                    ,denominator
                    ,numerator
                    ,inexactToExact
                    ,exactToInexact) where

import Lusp.LispError (LispError(NumArgs
                                ,TypeMismatch
                                ,DivBy0
                                ,Other))
import Lusp.LispVal (LispVal(Integer
                            ,Ratio
                            ,Real
                            ,Complex))

import Prelude hiding (div
                      ,subtract
                      ,compare
                      ,floor
                      ,ceiling
                      ,truncate
                      ,round
                      ,exp
                      ,log
                      ,sqrt)
import qualified Prelude as P (div
                              ,compare
                              ,floor
                              ,ceiling
                              ,truncate
                              ,round
                              ,exp
                              ,log
                              ,sqrt)

import Control.Exception (throw)
import Data.Complex (Complex((:+)))
import Data.List (sortBy)
import qualified Data.Ratio as D ((%)
                                  ,numerator
                                  ,denominator
                                  ,approxRational)

-- | Cast one of the numbers to the level of the numeric tower the other is at
numCast :: LispVal
        -- ^ Number
        -> LispVal
        -- ^ Number
        -> (LispVal, LispVal)
        -- ^ The numbers brought to the same level of the numeric tower
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
ratioToComplex (Ratio x) = Complex $ fromInteger (D.numerator x)
                                   / fromInteger (D.denominator x)
ratioToComplex _ = error "Expected Ratio"

-- | Throw an exception if the argument is not a number.
assertIsNumber :: LispVal -> LispVal
assertIsNumber x@(Integer _) = x
assertIsNumber x@(Ratio   _) = x
assertIsNumber x@(Real    _) = x
assertIsNumber x@(Complex _) = x
assertIsNumber x             = throw $ TypeMismatch "number" x

add :: [LispVal] -> LispVal
add []  = Integer 0
add [x] = assertIsNumber x
add params = foldl1 (\x y -> add' $ numCast x y) params
  where add' (Integer a, Integer b) = Integer $ a + b
        add' (Ratio   a, Ratio   b) = Ratio   $ a + b
        add' (Real    a, Real    b) = Real    $ a + b
        add' (Complex a, Complex b) = Complex $ a + b
        add' _ = error "Expected Number"

subtract :: [LispVal] -> LispVal
subtract []  = throw $ NumArgs "1" []
subtract [x] = subtract [Integer 0, assertIsNumber x]
subtract params = foldl1 (\x y -> sub $ numCast x y) params
  where sub (Integer a, Integer b) = Integer $ a - b
        sub (Ratio   a, Ratio   b) = Ratio   $ a - b
        sub (Real    a, Real    b) = Real    $ a - b
        sub (Complex a, Complex b) = Complex $ a - b
        sub _ = error "Expected Number"

multiply :: [LispVal] -> LispVal
multiply []  = Integer 1
multiply [x] = assertIsNumber x
multiply params = foldl1 (\x y -> mul $ numCast x y) params
  where mul (Integer a, Integer b) = Integer $ a * b
        mul (Ratio   a, Ratio   b) = Ratio   $ a * b
        mul (Real    a, Real    b) = Real    $ a * b
        mul (Complex a, Complex b) = Complex $ a * b
        mul _ = error "Expected Number"

divide :: [LispVal] -> LispVal
divide []  = throw $ NumArgs "1" []
divide [x] = divide [Integer 1, assertIsNumber x]
divide params = foldl1 (\x y -> div $ numCast x y) params
  where div (Integer a, Integer b)
            | b           == 0 = err
            | (a `mod` b) == 0 = Integer (a `P.div` b)
            | otherwise        = Ratio ((D.%) a b)
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

-- | Returns an 'Data.Ord.Ord' comparison of the two numbers.
-- Throws an error if one of the arguments is a Complex.
compare :: LispVal -> LispVal -> Ordering
compare a b = comp $ numCast a b
  where comp (Integer x, Integer y) = x `P.compare` y
        comp (Ratio   x, Ratio   y) = x `P.compare` y
        comp (Real    x, Real    y) = x `P.compare` y
        comp (Complex _, Complex _) = throw $ Other
          "Comparison between complex numebers is not allowed"
        comp _ = error "Expected Number"

equal :: LispVal -> LispVal -> Bool
equal x y = compare x y == EQ

equalElems :: [LispVal] -> Bool
equalElems xs = null xs || and ((head xs `equal`) <$> xs)

equalLists :: [LispVal] -> [LispVal] -> Bool
equalLists xs ys = (length xs == length ys) && and (zipWith equal xs ys)

increasing :: [LispVal] -> Bool
increasing xs = nonDecreasing xs && elemsUnique xs

decreasing :: [LispVal] -> Bool
decreasing xs = nonIncreasing xs && elemsUnique xs

nonDecreasing :: [LispVal] -> Bool
nonDecreasing xs = xs `equalLists` sortBy compare xs

nonIncreasing :: [LispVal] -> Bool
nonIncreasing xs = xs `equalLists` sortBy (flip compare) xs

elemsUnique :: [LispVal] -> Bool
elemsUnique [a, b]   = not (a `equal` b)
elemsUnique (a:b:cs) = not (a `equal` b) && elemsUnique (b:cs)
elemsUnique xs       = length xs < 2

numerator :: LispVal -> LispVal
numerator x@(Integer _) = x
numerator (Ratio x)     = Integer $ D.numerator x
numerator (Real x)    = (numerator . Ratio . doubleToRatio) x
numerator x             = throw $ TypeMismatch "integer / ratio / real" x

denominator :: LispVal -> LispVal
denominator (Integer _) = Integer 1
denominator (Ratio x)   = Integer $ D.denominator x
denominator (Real x)  = (denominator . Ratio . doubleToRatio) x
denominator x           = throw $ TypeMismatch "integer / ratio / real" x

unaryRealToIntOp :: (Double -> Integer) -> LispVal -> LispVal
unaryRealToIntOp _ x@(Integer _) = x
unaryRealToIntOp op (Ratio x)    = Integer $ op (ratioToDouble x)
unaryRealToIntOp op (Real x)     = Integer $ op x
unaryRealToIntOp _ x = throw $ TypeMismatch "integer / ratio / real" x

floor :: LispVal -> LispVal
floor = unaryRealToIntOp P.floor

ceiling :: LispVal -> LispVal
ceiling = unaryRealToIntOp P.ceiling

truncate :: LispVal -> LispVal
truncate = unaryRealToIntOp P.truncate

round :: LispVal -> LispVal
round = unaryRealToIntOp P.round

unaryRealToRealOp :: (Double -> Double) -> LispVal -> LispVal
unaryRealToRealOp op (Integer x)   = Real $ op (fromInteger x)
unaryRealToRealOp op (Ratio x)     = Real $ op (ratioToDouble x)
unaryRealToRealOp op (Real x)      = Real $ op x
unaryRealToRealOp _ x = throw $ TypeMismatch "integer / ratio / real" x

exp :: LispVal -> LispVal
exp = unaryRealToRealOp P.exp

sqrt :: LispVal -> LispVal
sqrt = unaryRealToRealOp P.sqrt

log :: LispVal -> LispVal
log = unaryRealToRealOp P.log

realBinOp :: (Double -> Double -> Double) -> LispVal -> LispVal -> LispVal
realBinOp _ x@(Complex _) _ = throw $ TypeMismatch "integer / ratio / real" x
realBinOp _ _ x@(Complex _) = throw $ TypeMismatch "integer / ratio / real" x
realBinOp oper a b          = rBOp oper (numCast a b)
  where rBOp :: (Double -> Double -> Double) -> (LispVal, LispVal) -> LispVal
        rBOp op (Integer x, Integer y) = Real $ fromInteger x `op` fromInteger y
        rBOp op (Ratio   x, Ratio   y) = Real $ ratioToDouble x `op`
                                                ratioToDouble y
        rBOp op (Real    x, Real    y) = Real $ x `op` y
        rBOp _ _ = error "Expected Number"

expt :: LispVal -> LispVal -> LispVal
expt = realBinOp (**)

exactToInexact :: LispVal -> LispVal
exactToInexact (Integer x)   = Real $ fromInteger x
exactToInexact (Ratio x)     = Real $ ratioToDouble x
exactToInexact x@(Real _)    = x
exactToInexact x@(Complex _) = x
exactToInexact x             = throw $ TypeMismatch "number" x

inexactToExact :: LispVal -> LispVal
inexactToExact x@(Integer _) = x
inexactToExact x@(Ratio _)   = x
inexactToExact (Real x)      = Ratio $ doubleToRatio x
inexactToExact x             = throw $ TypeMismatch "integer / ratio / real" x

ratioToDouble :: Rational -> Double
ratioToDouble x = fromInteger (D.numerator x) / fromInteger (D.denominator x)

doubleToRatio :: Double -> Rational
doubleToRatio x = D.approxRational x 0.00000001

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
integerBinDivOp [x, Integer _] _          = throw $ TypeMismatch "integer" x
integerBinDivOp xs _                      = throw $ NumArgs "2" xs
