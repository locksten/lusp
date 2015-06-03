import Lusp.Evaluate (evaluate
                     ,initialEnv)
import Lusp.LispVal (Env)
import Lusp.Parser (parse)
import qualified Paths (importDirs)
import Repl (repl
            ,showEval)

import Control.Monad (when)
import Data.List (find)
import Data.Maybe (fromMaybe)
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
    -- Run the repl if no arguments are provided
    null <$> getArgs >>= flip when (cwdEnv >>= repl >> exitSuccess)
    -- If the argument is not an option, treat it as a filename
    safeArg 0 >>= \arg -> fromMaybe (executeFile arg) (findOption arg)

-- | Executes the code contained within the file
executeFile :: FilePath -> IO ()
executeFile file = doesFileExist file >>= \exists ->
  if exists then do
        str <- readFile file
        args <- getArgs
        let fileDir = addTrailingPathSeparator $ normalise $ takeDirectory file
        env <- Paths.importDirs >>= \i ->
          addTrailingPathSeparator <$> getCurrentDirectory >>= \d ->
          initialEnv (fileDir : i) (d : tailOrEmpty args)
        execute env str
    else putStrLn ("The file \"" ++ file ++ "\" does not exist") >> exitFailure
  where tailOrEmpty xs = if null xs
                            then []
                            else tail xs

-- | Returns the n'th commandline argument.
-- If that's not possible, printsHelp and exits
safeArg :: Int -> IO String
safeArg n = (length <$> getArgs) >>= \len ->
    if len < n + 1 then printHelp >> exitSuccess
                   else (!! n) <$> getArgs

-- | An environment with no other commandline arguments except the cwd
cwdEnv :: IO Env
cwdEnv = addTrailingPathSeparator <$> getCurrentDirectory >>= \d ->
    Paths.importDirs >>= \i -> initialEnv (d : i) [d]

-- | Prints the version of the program
printVersion :: IO ()
printVersion = putStrLn ("lusp " ++ showVersion version)

-- | Prints the help
printHelp :: IO ()
printHelp = putStrLn "Usage: lusp [option]\n\
      \<<nothing>>           start the repl\n\
      \filename [args]       execute a file\n\
      \-e --evaluate \"expr\"  evaluate an expression\n\
      \-p --parse    \"expr\"  parse an expression\n\
      \-v --version          print the version\n\
      \-h --help             print this help"

-- | A list of cammandline options with corresponding IO actions
options :: [([String], IO ())]
options = [(["--help"
            ,"-h"], printHelp)
          ,(["--version"
            ,"-v"], printVersion)
          ,(["--evaluate"
            ,"-e"], (safeArg 1 >>=) . showEval =<< cwdEnv)
          ,(["--parse"
            ,"-p"], safeArg 1 >>= showParse)]

-- | Finds the commandline action corresponding to the option string
findOption :: String -> Maybe (IO ())
findOption str = case find (elem str . fst) options of
                   Nothing -> Nothing
                   Just x  -> Just $ snd x

-- | Parses and prints the code
showParse :: String -> IO ()
showParse = putStr . concatMap ((++ "\n") . show) . parse

-- | Executes the code returning nothing and printing nothing to stdout
execute :: Env -> String -> IO ()
execute env = (>> return ()) . evaluate env . parse
