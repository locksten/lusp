import Lusp.Evaluate (evaluate)
import Lusp.Parser (parse)

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure
                   ,exitSuccess)

main :: IO ()
main = safeArg 0 >>= \opt ->
    case opt of
      "-p"     -> safeArg 1 >>= showParse
      "-e"     -> safeArg 1 >>= showEval
      "-h"     -> usageAndExit
      "--help" -> usageAndExit
      file     -> doesFileExist file >>= \exists ->
        if exists then readFile file >>= execute
                  else putStrLn ("The file \"" ++ file ++ "\" does not exist")
                       >> exitFailure
  where safeArg n = (length <$> getArgs) >>= \len ->
          if len < n + 1 then usageAndExit
                         else (!! n) <$> getArgs
        usageAndExit = putStrLn "Usage: \n\
      \ filename    execute a file\n\
      \-e \"expr\"    evaluate an expression\n\
      \-p \"expr\"    parse an expression\n\
      \-h --help    print help and exit" >> exitSuccess

showParse :: String -> IO ()
showParse = print . parse

showEval :: String -> IO ()
showEval = (print' =<<) . evaluate . parse
  where print' = putStrLn . concatMap ((++ "  ") . show)

execute :: String -> IO ()
execute = (>> return ()) . evaluate . parse
