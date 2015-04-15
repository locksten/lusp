import Lusp.Evaluate (evaluate)
import Lusp.Parser (parse)

import Control.Monad (when)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    when (length args < 2) (error usage)
    let opt = args !! 0
    let str = args !! 1
    case opt of
      "-p" -> showParse str
      "-e" -> showEval str
      _    -> putStrLn usage
  where usage = "Usage: option string\noptions:\n-p  parse\n-e  evaluate"

showParse :: String -> IO ()
showParse = print . parse

showEval :: String -> IO ()
showEval = (print' =<<) . evaluate . parse
  where print' = putStrLn . concatMap ((++ "  ") . show)
