module Lusp.Primitives (primitives
                       ,ioPrimitives
                       ,primitiveEnv
                       ,write) where

import Lusp.Environment (emptyEnv
                        ,bindVars)
import qualified Lusp.Eval as Eval (apply)
import Lusp.LispError (LispError(NumArgs
                                ,TypeMismatch
                                ,Other))
import Lusp.LispVal (LispVal(List
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

primitiveEnv :: IO Env
primitiveEnv = emptyEnv >>=
    flip bindVars ((makeF IOFunc <$> ioPrimitives) ++
                   (makeF PrimitiveFunc <$> primitives))
  where makeF cons (var, func) = (var, cons func)

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
             ,("inexact->exact" ,singleArg N.inexactToExact)
             ,("exact->inexact" ,singleArg N.exactToInexact)
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
             ,("current-input-port"  ,argumentless $ Port stdin)
             ,("current-output-port" ,argumentless $ Port stdout)
             ,("eof-object?" ,predicate LVU.isEOF)
             ,("number?"     ,predicate isNumber)
             ,("complex?"    ,predicate isComplex)
             ,("real?"       ,predicate isReal)
             ,("rational?"   ,predicate isRatio)
             ,("integer?"    ,predicate isInteger)
             ,("rational?"   ,predicate isRatio)
             ,("exact?"      ,predicate isExact)
             ,("inexact?"    ,predicate (not . isExact))]

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

numberTypePredicates :: [LispVal -> Bool]
numberTypePredicates = [LVU.isInteger, LVU.isRatio, LVU.isReal, LVU.isComplex]

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
