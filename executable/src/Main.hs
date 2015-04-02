import SchemeParser
import System.Environment
import Control.Monad
import Control.Applicative ((<$>), (<*), (*>), (<*>))

main :: IO ()
main = getArgs >>= (\args -> unless (length args < 1) $ putStrLn $
       init <$> unlines . map show <$> parseExpressions $ head args)
