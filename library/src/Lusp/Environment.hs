module Lusp.Environment (emptyEnv
                        ,getVar
                        ,setVar
                        ,defineVar) where

import Lusp.LispError (LispError(UnboundVar))
import Lusp.LispVal (LispVal(Void)
                    ,Env)

import Control.Exception (throw)
import Data.IORef (newIORef, readIORef, writeIORef)

emptyEnv :: IO Env
emptyEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound env var = maybe False (const True) . lookup var <$> readIORef env

getVar :: Env -> String -> IO LispVal
getVar env var = readIORef env >>= \env' ->
    maybe (throw $ UnboundVar "Getting an unboud variable" var)
    (readIORef) (lookup var env')

setVar :: Env -> String -> LispVal -> IO LispVal
setVar env var value = readIORef env >>= \env' ->
    maybe (throw $ UnboundVar "Setting an unbound variable" var)
    (flip writeIORef value) (lookup var env')
    >> return Void

defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar env var value = isBound env var >>= \alreadyDefined ->
    if alreadyDefined
       then setVar env var value
       else do newValue <- newIORef value
               env' <- readIORef env
               writeIORef env ((var, newValue):env')
    >> return Void

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars env bindings = readIORef env >>= extendEnv bindings >>= newIORef
  where extendEnv bindings' env' = (++ env') <$> (mapM addBinding bindings')
        addBinding (var, value) = (\x -> (var, x)) <$> newIORef value
