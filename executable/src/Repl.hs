module Repl (repl
            ,showEval) where

import Lusp.Evaluate (evaluate
                     ,autoCompleteTokens)
import Lusp.LispError (LispError)
import Lusp.LispVal (Env)
import Lusp.Parser (parse)
import qualified Paths (replHistory)

import Control.Exception (IOException
                         ,Handler(Handler)
                         ,catches)
import Control.Monad (unless)
import System.FilePath (takeDirectory)
import System.Console.Readline (readline
                               ,filenameCompletionFunction
                               ,setCompletionEntryFunction
                               ,addHistory)
import System.Directory (doesFileExist
                        ,createDirectoryIfMissing)
import System.IO (openFile
                 ,hClose
                 ,IOMode(WriteMode))

-- | Executes the code and prints the results to stdout
showEval :: Env -> String -> IO ()
showEval env = (print' =<<) . evaluate env . parse
  where print' = putStrLn . concatMap ((++ " ") . show)

-- | Starts the repl
repl :: Env -> IO ()
repl env = (Paths.replHistory >>= createFile) >> readHistory >> replStep env

-- | Reads Evaluates Prints Loops
replStep :: Env -> IO ()
replStep env = do
    setCompletionEntryFunction $ Just $ completer env
    maybeLine <- readline promptStr
    case maybeLine of
      Nothing   -> return ()
      Just line -> unless (line `elem` quitCommands) $ run line
  where run line = do
          showEval env line `catches` [Handler handleIO
                                      ,Handler handleLisp]
          saveHistory line
          replStep env
        handleIO :: IOException -> IO ()
        handleIO = print
        handleLisp :: LispError -> IO ()
        handleLisp = print

-- | Reads the Paths.replHistory file into readline's history
readHistory :: IO ()
readHistory = sequence_ . fmap addHistory . lines =<<
    (readFile =<< Paths.replHistory)

-- | saves a line in the readline history and in the Paths.replHistory file
saveHistory :: String -> IO ()
saveHistory l = do
    addHistory l
    Paths.replHistory >>= flip appendFile (l ++ "\n")

-- | Creates an empty file if it does not yet exist
createFile :: FilePath -> IO ()
createFile path = doesFileExist path >>= flip unless
    (createDirectoryIfMissing True (takeDirectory path) >>
    openFile path WriteMode >>= hClose)

-- | Autocompletes paths and keywords
completer :: Env -> String -> IO [String]
completer env k = filter match <$> list
  where match = (k ==) . take (length k)
        list  = (++) <$> autoCompleteTokens env <*> filenameCompletionFunction k

promptStr :: String
promptStr = "Lusp> "

quitCommands :: [String]
quitCommands = ["q"
               ,":q"
               ,"quit"
               ,"exit"]
