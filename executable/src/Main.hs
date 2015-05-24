import Lusp.Evaluate (evaluate
                     ,initialEnv)
import Lusp.LispVal (Env)
import Lusp.Parser (parse)

import System.Console.Readline (readline
                               ,addHistory)
import System.Directory (doesFileExist
                        ,getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure
                   ,exitSuccess)

main :: IO ()
main = do
    opt <- safeArg 0
    let initEnv = getCurrentDirectory >>= \d -> initialEnv d []
    case opt of
      "-r"     -> initEnv >>= repl
      "-p"     -> safeArg 1 >>= showParse
      "-e"     -> (safeArg 1 >>=) . showEval =<< initEnv
      "-h"     -> usageAndExit
      "--help" -> usageAndExit
      file     -> doesFileExist file >>= \exists ->
        if exists then do
                  str <- readFile file
                  args <- getArgs
                  cwd <- getCurrentDirectory
                  env <- initialEnv file (cwd : tail args)
                  execute env str
                  else putStrLn ("The file \"" ++ file ++ "\" does not exist")
                       >> exitFailure
  where safeArg n = (length <$> getArgs) >>= \len ->
          if len < n + 1 then usageAndExit
                         else (!! n) <$> getArgs
        usageAndExit = putStrLn "Usage: \n\
      \ filename args  execute a file\n\
      \-r              start the repl\n\
      \-e \"expr\"       evaluate an expression\n\
      \-p \"expr\"       parse an expression\n\
      \-h --help       print help and exit" >> exitSuccess

showParse :: String -> IO ()
showParse = print . parse

-- | Executes the code returning nothing and printing nothing to stdout
execute :: Env -> String -> IO ()
execute env = (>> return ()) . evaluate env . parse

-- | Executes the code and prints the results to stdout
showEval :: Env -> String -> IO ()
showEval env = (print' =<<) . evaluate env . parse
  where print' = putStrLn . concatMap ((++ "  ") . show)

-- | Reads Evaluates Prints Loops
repl :: Env -> IO ()
repl env = do
    maybeLine <- readline "Lusp> "
    case maybeLine of
      Nothing     -> return ()
      Just "exit" -> return ()
      Just line   -> do addHistory line
                        showEval env line
                        repl env
