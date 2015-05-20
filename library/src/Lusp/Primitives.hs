module Lusp.Primitives (primitives
                       ,ioPrimitives
                       ,primitiveEnv) where

import Lusp.Environment (emptyEnv
                        ,bindVars)
import qualified Lusp.Eval as Eval (apply)
import Lusp.LispError (LispError(NumArgs
                                ,TypeMismatch
                                ,ErrorCommand
                                ,Other))
import Lusp.LispVal (LispVal(List
                            ,DottedList
                            ,Integer
                            ,Real
                            ,Ratio
                            ,Complex
                            ,Atom
                            ,Vector
                            ,Bool
                            ,String
                            ,Char
                            ,PrimitiveFunc
                            ,IOFunc
                            ,Port
                            ,Void
                            ,EOF)
                    ,Env)
import qualified Lusp.LispValUtils as LVU (prettyPrint
                                          ,extractList
                                          ,isSymbol
                                          ,isEOF
                                          ,isInteger
                                          ,isReal
                                          ,isRatio
                                          ,isComplex)
import qualified Lusp.Numeric as N (equalElems
                                   ,increasing
                                   ,decreasing
                                   ,nonIncreasing
                                   ,nonDecreasing
                                   ,sqrt
                                   ,exp
                                   ,log
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
                                   ,numerator
                                   ,denominator
                                   ,inexactToExact
                                   ,exactToInexact)
import Lusp.Parser (parse)

import Prelude hiding (read
                      ,map
                      ,compare)

import Control.Exception (throw
                         ,catch)
import Data.List (transpose)
import System.IO (IOMode(ReadMode
                        ,WriteMode)
                 ,Handle
                 ,hIsReadable
                 ,hIsWritable
                 ,openFile
                 ,hClose
                 ,hFlush
                 ,hGetLine
                 ,hGetChar
                 ,hLookAhead
                 ,hReady
                 ,stdin
                 ,stdout
                 ,hPutStr)
import System.IO.Error (isEOFError)

-- | Create an Environment containing primitve functions
primitiveEnv :: IO Env
primitiveEnv = emptyEnv >>=
    flip bindVars ((makeF IOFunc <$> ioPrimitives) ++
                   (makeF PrimitiveFunc <$> primitives))
  where makeF constr (var, func) = (var, constr func)

-- | A list of primitve functions
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+"  ,N.add)
             ,("-"  ,N.subtract)
             ,("*"  ,N.multiply)
             ,("/"  ,N.divide)
             ,("="  ,listPredicate N.equalElems)
             ,("<"  ,listPredicate N.increasing)
             ,(">"  ,listPredicate N.decreasing)
             ,("<=" ,listPredicate N.nonDecreasing)
             ,(">=" ,listPredicate N.nonIncreasing)
             ,("numerator"   ,singleArg N.numerator)
             ,("denominator" ,singleArg N.denominator)
             ,("floor"       ,singleArg N.floor)
             ,("ceiling"     ,singleArg N.ceiling)
             ,("truncate"    ,singleArg N.truncate)
             ,("round"       ,singleArg N.round)
             ,("sqrt"        ,singleArg N.sqrt)
             ,("log"         ,singleArg N.log)
             ,("exp"         ,singleArg N.exp)
             ,("expt"        ,twoArg N.expt)
             ,("modulo"      ,N.modulo)
             ,("remainder"   ,N.remainder)
             ,("quotient"    ,N.quotient)
             ,("eof-object?" ,predicate LVU.isEOF)
             ,("symbol?"     ,predicate LVU.isSymbol)
             ,("number?"     ,predicate isNumber)
             ,("complex?"    ,predicate isComplex)
             ,("real?"       ,predicate isReal)
             ,("rational?"   ,predicate isRatio)
             ,("integer?"    ,predicate isInteger)
             ,("rational?"   ,predicate isRatio)
             ,("exact?"      ,predicate isExact)
             ,("inexact?"    ,predicate (not . isExact))
             ,("pair?"       ,predicate isPair)
             ,("eqv?"        ,binaryPredicate eqv)
             ,("eq?"         ,binaryPredicate eqv)
             ,("equal?"      ,binaryPredicate eqv)
             ,("inexact->exact" ,singleArg N.inexactToExact)
             ,("exact->inexact" ,singleArg N.exactToInexact)
             ,("symbol->string" ,singleArg symbolToString)
             ,("string->symbol" ,singleArg stringToSymbol)
             ,("current-input-port"  ,argumentless $ Port stdin)
             ,("current-output-port" ,argumentless $ Port stdout)
             ,("cons"     ,twoArg cons)
             ,("car"      ,singleArg car)
             ,("cdr"      ,singleArg cdr)]

-- | A list of primitive IO functions
ioPrimitives :: [(String, [LispVal] -> IO LispVal)]
ioPrimitives = [("open-input-file"   ,makePort ReadMode)
               ,("open-output-file"  ,makePort WriteMode)
               ,("close-input-port"  ,closePort)
               ,("close-output-port" ,closePort)
               ,("input-port?"  ,ioPredicate isInputPort)
               ,("output-port?" ,ioPredicate isOutputPort)
               ,("read"         ,input read)
               ,("read-char"    ,input $ inputChar hGetChar)
               ,("peek-char"    ,input $ inputChar hLookAhead)
               ,("char-ready?"  ,input charReady)
               ,("write"        ,output write)
               ,("write-char"   ,output writeChar)
               ,("display"      ,output display)
               ,("newline"      ,outputConstant "\n")
               ,("error"        ,errorCommand)
               ,("apply" ,apply)
               ,("map"   ,map)]

argumentless :: LispVal -> [LispVal] -> LispVal
argumentless x [] = x
argumentless _ x  = throw $ NumArgs "0" x

singleArg :: (LispVal -> LispVal) -> [LispVal] -> LispVal
singleArg op [x] = op x
singleArg _ x    = throw $ NumArgs "1" x

twoArg :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> LispVal
twoArg op [x, y] = op x y
twoArg _ x       = throw $ NumArgs "2" x

listPredicate :: ([LispVal] -> Bool) -> [LispVal] -> LispVal
listPredicate op xs = if length xs >= 2
                         then Bool $ op xs
                         else throw $ NumArgs "2 or more" xs

predicate :: (LispVal -> Bool) -> [LispVal] -> LispVal
predicate pred' [x] = Bool $ pred' x
predicate _ x       = throw $ NumArgs "1" x

binaryPredicate :: (LispVal -> LispVal -> Bool) -> [LispVal] -> LispVal
binaryPredicate pred' [x, y] = Bool $ pred' x y
binaryPredicate _ x          = throw $ NumArgs "2" x

eqv :: LispVal -> LispVal -> Bool
eqv (List a) (List b) = length a == length b && and (zipWith eqv a b)
eqv (DottedList as a) (DottedList bs b) = eqv (List as) (List bs) && eqv a b
eqv (Vector a) (Vector b) = length a == length b && and (zipWith eqv a b)
eqv (Bool a) (Bool b) = a == b
eqv (Atom a) (Atom b) = a == b
eqv (Char a) (Char b) = a == b
eqv (String a) (String b) = a == b
eqv a@(Integer _) b@(Integer _) = N.equalElems [a, b]
eqv a@(Ratio _)   b@(Ratio _)   = N.equalElems [a, b]
eqv a@(Integer _) b@(Ratio _)   = N.equalElems [a, b]
eqv a@(Ratio _)   b@(Integer _) = N.equalElems [a, b]
eqv a@(Real _)    b@(Real _)    = N.equalElems [a, b]
eqv a@(Complex _) b@(Complex _) = N.equalElems [a, b]
eqv a@(Complex _) b@(Real _)    = N.equalElems [a, b]
eqv a@(Real _)    b@(Complex _) = N.equalElems [a, b]
eqv _ _                         = False

isNumber :: LispVal -> Bool
isNumber x = or $ ($ x) <$> numberTypePredicates

isInteger :: LispVal -> Bool
isInteger x = or $ ($ x) <$> take 1 numberTypePredicates

isRatio :: LispVal -> Bool
isRatio x = or $ ($ x) <$> take 2 numberTypePredicates

isReal :: LispVal -> Bool
isReal x = or $ ($ x) <$> take 3 numberTypePredicates

isComplex :: LispVal -> Bool
isComplex x = or $ ($ x) <$> take 4 numberTypePredicates

isExact :: LispVal -> Bool
isExact x = if isNumber x then isInteger x || isRatio x
                          else throw $ TypeMismatch "number" x

-- | A list of predicates representing the numeric tower of Lisp
numberTypePredicates :: [LispVal -> Bool]
numberTypePredicates = [LVU.isInteger, LVU.isRatio, LVU.isReal, LVU.isComplex]

symbolToString :: LispVal -> LispVal
symbolToString (Atom x) = String x
symbolToString x        = throw $ TypeMismatch "symbol" x

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String x) = Atom x
stringToSymbol x          = throw $ TypeMismatch "string" x

isPair :: LispVal -> Bool
isPair (List xs)         = (not . null) xs
isPair (DottedList xs _) = isPair $ List xs
isPair _                 = False

cons :: LispVal -> LispVal -> LispVal
cons x (List [])         = List [x]
cons x (List xs)         = List (x : xs)
cons x (DottedList xs e) = DottedList (x : xs) e
cons x y                 = DottedList [x] y

car :: LispVal -> LispVal
car (List (x:_))     = x
car (DottedList xs _) = car $ List xs
car x                 = throw $ TypeMismatch "pair" x

cdr :: LispVal -> LispVal
cdr (List (_:xs))         = List xs
cdr (DottedList [_] e)    = e
cdr (DottedList (_:xs) e) = DottedList xs e
cdr x                     = throw $ TypeMismatch "pair" x

makePort :: IOMode -> [LispVal] -> IO LispVal
makePort mode [String filename] = Port <$> openFile filename mode
makePort _ [x]                  = throw $ TypeMismatch "String" x
makePort _ x                    = throw $ NumArgs "1" x

closePort :: [LispVal] -> IO LispVal
closePort [Port port] = hClose port >> return Void
closePort [x]         = throw $ TypeMismatch "<IO port>" x
closePort x           = throw $ NumArgs "1" x

input :: (Handle -> IO LispVal) -> [LispVal] -> IO LispVal
input op []          = input op [Port stdin]
input op [Port port] = catch (op port) care
  where care :: IOError -> IO LispVal
        care e = if isEOFError e then return EOF else throw e
input _ [x]          = throw $ TypeMismatch "<IO port>" x
input _ x            = throw $ NumArgs "0 or 1" x

read :: Handle -> IO LispVal
read hdl = (top . parse) <$> hGetLine hdl
  where top xs = if null xs then EOF else head xs

inputChar :: (Handle -> IO Char) -> Handle -> IO LispVal
inputChar op hdl = hReady hdl >>= \ready ->
    if ready then Char <$> op hdl else return EOF

charReady :: Handle -> IO LispVal
charReady hdl = Bool <$> catch (hReady hdl) care
  where care :: IOError -> IO Bool
        care e = if isEOFError e then return True else throw e

outputConstant :: String -> [LispVal] -> IO LispVal
outputConstant s []          = outputConstant s [Port stdout]
outputConstant s [Port port] = hPutStr port s >> hFlush port >> return Void
outputConstant _ [x]         = throw $ TypeMismatch "<IO port>" x
outputConstant _ x           = throw $ NumArgs "0 or 1" x

output :: (Handle -> LispVal -> IO ()) -> [LispVal] -> IO LispVal
output op [obj]            = output op [obj, Port stdout]
output op [obj, Port port] = op port obj >> hFlush port >> return Void
output _  [_, x]           = throw $ TypeMismatch "<IO port>" x
output _  x                = throw $ NumArgs "1 or 2" x

write :: Handle -> LispVal -> IO ()
write hdl = hPutStr hdl . show

display :: Handle -> LispVal -> IO ()
display hdl = hPutStr hdl . LVU.prettyPrint

errorCommand :: [LispVal] -> IO LispVal
errorCommand [] = (throw $ ErrorCommand $ LVU.prettyPrint $ String "")
    >> return Void
errorCommand [msg] = (throw $ ErrorCommand $ LVU.prettyPrint msg) >> return Void
errorCommand x = throw $ NumArgs "0 or 1" x

writeChar :: Handle -> LispVal -> IO ()
writeChar hdl (Char c) = hPutStr hdl [c]
writeChar _ x          = throw $ TypeMismatch "Char" x

ioPredicate :: (LispVal -> IO Bool) -> [LispVal] -> IO LispVal
ioPredicate pred' [x] = Bool <$> pred' x
ioPredicate _ x       = throw $ NumArgs "1" x

isInputPort :: LispVal -> IO Bool
isInputPort (Port x) = hIsReadable x
isInputPort _        = return False

isOutputPort :: LispVal -> IO Bool
isOutputPort (Port x) = hIsWritable x
isOutputPort _        = return False

apply :: [LispVal] -> IO LispVal
apply [f, List args] = Eval.apply f args
apply [_, x]         = throw $ TypeMismatch "list" x
apply x              = throw $ NumArgs "2" x

map :: [LispVal] -> IO LispVal
map (func:lists@(_:_)) =
    if assertLengths then List <$> sequence (Eval.apply func <$> transposed)
                     else throw $ Other "map: The lists must be of equal length"
  where transposed    = transpose extracted
        extracted     = LVU.extractList <$> lists
        lengths       = length <$> extracted
        assertLengths = and $ (== head lengths) <$> lengths
map x = throw $ NumArgs "2 or more" x
