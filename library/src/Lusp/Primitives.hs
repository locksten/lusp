module Lusp.Primitives (primitives
                       ,ioPrimitives
                       ,primitiveEnv) where

import Lusp.Environment (emptyEnv
                        ,bindVars)
import qualified Lusp.Eval as Eval (apply)
import Lusp.LispError (LispError(NumArgs
                                ,TypeMismatch))
import Lusp.LispVal (LispVal(List
                            ,Bool
                            ,String
                            ,PrimitiveFunc
                            ,IOFunc
                            ,Port
                            ,Void)
                    ,Env)
import qualified Lusp.Numeric as N (add
                                   ,subtract
                                   ,multiply
                                   ,divide
                                   ,modulo
                                   ,remainder
                                   ,quotient)
import Lusp.Parser (parse)

import Prelude hiding (read)

import Control.Exception (throw)
import System.IO (IOMode(ReadMode
                        ,WriteMode)
                 ,Handle
                 ,hIsReadable
                 ,hIsWritable
                 ,openFile
                 ,hClose
                 ,hGetLine
                 ,hPrint
                 ,stdin
                 ,stdout)

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
             ,("current-output-port", argumentless $ Port stdout)]

ioPrimitives :: [(String, [LispVal] -> IO LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode)
               ,("open-output-file", makePort WriteMode)
               ,("close-input-port", closePort)
               ,("close-output-port", closePort)
               ,("input-port?", portPredicate hIsReadable)
               ,("output-port?", portPredicate hIsWritable)
               ,("read", read)
               ,("write", write)
               ,("apply", apply)]

argumentless :: LispVal -> [LispVal] -> LispVal
argumentless x [] = x
argumentless _ x = throw $ NumArgs [0] x

makePort :: IOMode -> [LispVal] -> IO LispVal
makePort mode [String filename] = Port <$> openFile filename mode
makePort _ x                    = throw $ NumArgs [1] x

closePort :: [LispVal] -> IO LispVal
closePort [Port port] = hClose port >> return Void
closePort _           = return Void

read :: [LispVal] -> IO LispVal
read []          = read [Port stdin]
read [Port port] = (head . parse) <$> hGetLine port
read [x]         = throw $ TypeMismatch "<IO port>" x
read x           = throw $ NumArgs [1, 2] x

write :: [LispVal] -> IO LispVal
write [obj]            = write [obj, Port stdout]
write [obj, Port port] = hPrint port obj >> return Void
write [_, x]           = throw $ TypeMismatch "<IO port>" x
write x                = throw $ NumArgs [1, 2] x

apply :: [LispVal] -> IO LispVal
apply [f, List args] = Eval.apply f args
apply [_, x]         = throw $ TypeMismatch "list" x
apply x              = throw $ NumArgs [2] x

portPredicate :: (Handle -> IO Bool) -> [LispVal] -> IO LispVal
portPredicate predicate [Port x] = Bool <$> predicate x
portPredicate _ [_]              = return $ Bool False
portPredicate _ x                = throw $ NumArgs [1] x
