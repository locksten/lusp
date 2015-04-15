module Lusp.Primitives (primitives
                       ,ioPrimitives
                       ,primitiveEnv
                       ,write) where

import Lusp.Environment (emptyEnv
                        ,bindVars)
import qualified Lusp.Eval as Eval (apply)
import Lusp.LispError (LispError(NumArgs
                                ,TypeMismatch))
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
import Lusp.LispValUtils (prettyPrint
                         ,isEOF)
import qualified Lusp.Numeric as N (add
                                   ,subtract
                                   ,multiply
                                   ,divide
                                   ,modulo
                                   ,remainder
                                   ,quotient)
import Lusp.Parser (parse)

import Prelude hiding (read)

import Control.Exception (throw
                         ,catch)
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
primitiveEnv = emptyEnv >>= flip bindVars (map (makeF IOFunc) ioPrimitives ++
                                           map (makeF PrimitiveFunc) primitives)
  where makeF cons (var, func) = (var, cons func)

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", N.add)
             ,("-", N.subtract)
             ,("*", N.multiply)
             ,("/", N.divide)
             ,("modulo", N.modulo)
             ,("remainder", N.remainder)
             ,("quotient", N.quotient)
             ,("current-input-port", argumentless $ Port stdin)
             ,("current-output-port", argumentless $ Port stdout)
             ,("eof-object?", predicate isEOF)]

ioPrimitives :: [(String, [LispVal] -> IO LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode)
               ,("open-output-file", makePort WriteMode)
               ,("close-input-port", closePort)
               ,("close-output-port", closePort)
               ,("input-port?", ioPredicate isInputPort)
               ,("output-port?", ioPredicate isOutputPort)
               ,("read", input read)
               ,("read-char", input $ inputChar hGetChar)
               ,("peek-char", input $ inputChar hLookAhead)
               ,("write", output write)
               ,("write-char", output writeChar)
               ,("char-ready?", input charReady)
               ,("display", output display)
               ,("newline", outputConstant "\n")
               ,("apply", apply)]

argumentless :: LispVal -> [LispVal] -> LispVal
argumentless x [] = x
argumentless _ x = throw $ NumArgs "0" x

predicate :: (LispVal -> Bool) -> [LispVal] -> LispVal
predicate pred' [x] = Bool $ pred' x
predicate _ x       = throw $ NumArgs "1" x

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
display hdl = hPutStr hdl . prettyPrint

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
