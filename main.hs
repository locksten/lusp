import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Applicative ((<$>))
import Text.Parsec.Combinator

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

main :: IO ()
main = getArgs >>= (\args -> unless (length args < 1) $ putStrLn $ readExpr $ head args)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseListOrDottedList


readExpr :: String -> String
readExpr input = case parse parseExpr "readExprRoot" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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
    String <$> between (char '"') (char '"') (many chars)
      where chars = escaped <|> noneOf "\""
            escaped = char '\\' >> choice (zipWith escChar characters replacements)
            escChar character replacement = char character >> return replacement
            characters   = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
            replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseList :: Parser [LispVal]
parseList = sepEndBy parseExpr spaces

parseDottedList :: Parser (Maybe LispVal)
parseDottedList = optionMaybe (char '.' >> spaces >> parseExpr)

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
    _ <- char '('
    list <- parseList
    end <- parseDottedList
    _ <- char ')'
    return $ case end of
      Nothing -> List list
      Just x -> DottedList list x

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >>
              parseExpr >>= \ x ->
              return $ List [Atom "quote", x]
