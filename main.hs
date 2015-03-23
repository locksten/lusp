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
main = getArgs >>= (\args -> unless (length args < 1) $ putStrLn $
       readExpr $ head args)

prettyPrintIndentLevel :: Int
prettyPrintIndentLevel = 4

prettyPrintList :: (LispVal, Int) -> String
prettyPrintList (xs, depth) = case xs of
    (List xs) -> indent depth ++ "( " ++ concatMap iteration xs ++ ") "
      where iteration xs = prettyPrintList (xs, depth + 1)
            indent depth = if depth == 0 then "" else "\n" ++ (concat $
                           replicate (depth * prettyPrintIndentLevel) " ")
    _         -> prettyPrint xs

prettyPrint :: LispVal -> String
prettyPrint (List xs) = prettyPrintList (List xs, 0)
prettyPrint (DottedList xs end) = "( " ++ concatMap prettyPrint xs ++ ". " ++
                                  prettyPrint end ++ ") "
prettyPrint (Atom x) = (init $ tail $ show x) ++ " "
prettyPrint (Number x) = show x ++ " "
prettyPrint (String x) = show x ++ " "
prettyPrint (Bool x) = if x then "#t " else "#f "

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseStartingWithOctothorpe
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseListOrDottedList

readExpr :: String -> String
readExpr input = case parse (sepBy parseExpr space) "readExprRoot" input of
    Left err -> "No match: " ++ show err
    Right val -> unlines (map prettyPrint val)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseStartingWithOctothorpe :: Parser LispVal
parseStartingWithOctothorpe = char '#' >>
                              parseBool

parseBool :: Parser LispVal
parseBool = Bool . (== 't') <$> oneOf "tf"

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
parseString = String <$> between (char '"') (char '"') (many chars)
  where chars = escaped <|> noneOf "\""
        escaped = char '\\' >> choice (zipWith escChar characters replacements)
        escChar character replacement = char character >> return replacement
        characters   = [ 'b',  'n',  'f',  'r',  't', '\\', '\"', '/']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseList :: Parser [LispVal]
parseList = sepEndBy parseExpr spaces

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
    _ <- char '('
    list <- parseList
    end <- optionMaybe (char '.' >> spaces >> parseExpr)
    _ <- char ')'
    return $ case end of
               Nothing -> List list
               Just x -> DottedList list x

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >>
              parseExpr >>= \ x ->
              return $ List [Atom "quote", x]
