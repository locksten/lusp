module Lusp.Environment (emptyEnv
                        ,childEnv
                        ,getVar
                        ,setVar
                        ,defineVar
                        ,bindVars) where

import Lusp.LispError (LispError(UnboundVar))
import Lusp.LispVal (LispVal(Void)
                    ,Env(Env))

import Control.Exception (throw)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map (member
                                        ,empty
                                        ,lookup
                                        ,insert
                                        ,fromList
                                        ,toList)

emptyEnv :: IO Env
emptyEnv =  (\e -> Env (Nothing, e)) <$> newIORef Map.empty

childEnv :: Env -> IO Env
childEnv parent = (\e -> Env (Just parent, e)) <$> newIORef Map.empty

isBound :: Env -> String -> IO Bool
isBound (Env(parent, env)) var = do
    inCurrent <- Map.member var <$> readIORef env
    inParent <- case parent of Nothing -> return False
                               Just p  -> isBound p var
    return (inCurrent || inParent)

getVar :: Env -> String -> IO LispVal
getVar (Env(parent, env)) var = readIORef env >>= \curr ->
    maybe tryParent readIORef (Map.lookup var curr)
  where tryParent = case parent of
            Nothing -> throw $ UnboundVar "Getting an unboud variable" var
            Just p  -> getVar p var

setVar :: Env -> String -> LispVal -> IO LispVal
setVar (Env(parent, env)) var value = readIORef env >>= \curr ->
    maybe tryParent (\x -> (x `writeIORef` value) >> return Void)
    (Map.lookup var curr)
  where tryParent = case parent of
            Nothing -> throw $ UnboundVar "Setting an unbound variable" var
            Just p  -> setVar p var value

defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar env@(Env(_, curr)) var value = isBound env var >>= \alreadyDefined ->
    if alreadyDefined
       then setVar env var value
       else do newValue <- newIORef value
               curr' <- readIORef curr
               writeIORef curr (Map.insert var newValue curr')
    >> return Void

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars (Env(parent, env)) bindings = readIORef env >>= doExtend >>=
        (toFullEnv <$>) . newIORef
  where doExtend env' = Map.fromList <$> extendEnv bindings (Map.toList env')
        extendEnv bindings' env' = (++ env') <$> mapM addBinding bindings'
        addBinding (var, value) = (\x -> (var, x)) <$> newIORef value
        toFullEnv x = Env (parent, x)
