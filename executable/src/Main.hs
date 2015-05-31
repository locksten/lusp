import Lusp.Evaluate (evaluate
                     ,initialEnv)
import Lusp.LispVal (Env)
import Lusp.Parser (parse)
import qualified Paths (importDirs)
import Repl (repl
            ,showEval)
import Data.Version (showVersion)
import System.FilePath (takeDirectory
                       ,normalise
                       ,addTrailingPathSeparator)
import Paths_lusp (version)
import System.Directory (doesFileExist
                        ,getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure
                   ,exitSuccess)

main :: IO ()
main = do
    opt <- safeArg 0
    cwd <- addTrailingPathSeparator <$> getCurrentDirectory
    let initEnv = Paths.importDirs >>= \i -> initialEnv (cwd : i) [cwd]
    case opt of
      "-r"        -> initEnv >>= repl
      "-p"        -> safeArg 1 >>= showParse
      "-e"        -> (safeArg 1 >>=) . showEval =<< initEnv
      "-h"        -> usageAndExit
      "--help"    -> usageAndExit
      "-v"        -> versionAndExit
      "--version" -> versionAndExit
      file     -> doesFileExist file >>= \exists ->
        if exists then do
                  str <- readFile file
                  args <- getArgs
                  let fileDir = addTrailingPathSeparator $ normalise $
                                takeDirectory file
                  env <- Paths.importDirs >>= \i ->
                    initialEnv (fileDir : i) (cwd : tailOrEmpty args)
                  execute env str
                  else putStrLn ("The file \"" ++ file ++ "\" does not exist")
                       >> exitFailure
  where safeArg n = (length <$> getArgs) >>= \len ->
          if len < n + 1 then usageAndExit
                         else (!! n) <$> getArgs
        tailOrEmpty xs = if null xs then [] else tail xs
        versionAndExit = putStrLn ("lusp " ++ showVersion version)
            >> exitSuccess
        usageAndExit = putStrLn "Usage: \n\
      \ filename args  execute a file\n\
      \-r              start the repl\n\
      \-e \"expr\"       evaluate an expression\n\
      \-p \"expr\"       parse an expression\n\
      \-v --version    print the version and exit\n\
      \-h --help       print help and exit" >> exitSuccess

-- | Parses and prints the code
showParse :: String -> IO ()
showParse = putStr . concatMap ((++ "\n") . show) . parse

-- | Executes the code returning nothing and printing nothing to stdout
execute :: Env -> String -> IO ()
execute env = (>> return ()) . evaluate env . parse
