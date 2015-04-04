import Lesp.Eval
import Lesp.LispVal
import Lesp.Parser

import Control.Applicative ((<$>), (<*), (*>), (<*>))
import Control.Monad
import System.Environment

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
showParse = putStrLn . show . parseExpressions

showEval :: String -> IO ()
showEval = putStrLn . show . (map eval) . parseExpressions
