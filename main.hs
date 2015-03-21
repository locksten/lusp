import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Applicative ((<$>))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

main :: IO ()
main = do
    args <- getArgs
    unless (length args < 1) $ putStrLn $ readExpr $ head args

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "readExprRoot" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    ret <- many chars
    _ <- char '"'
    return $ String ret
      where chars = escaped <|> noneOf "\""
            escaped = char '\\' >> choice (zipWith escChar characters replacements)
            escChar character replacement = char character >> return replacement
            characters   = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
            replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
        head' <- endBy parseExpr spaces
        tail' <- char '.' >> spaces >> parseExpr
        return $ DottedList head' tail'

parseQuoted :: Parser LispVal
parseQuoted = do
        char '\''
        x <- parseExpr
        return $ List [Atom "quote", x]
