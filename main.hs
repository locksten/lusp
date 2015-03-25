import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Applicative ((<$>))
import Data.Char
import Numeric
import Text.Parsec.Token (float)
import Data.List (findIndices)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Char Char
             | Bool Bool
             deriving (Show)

main :: IO ()
main = getArgs >>= (\args -> unless (length args < 1) $ putStrLn $
       init <$> readExpr $ head args)

prettyPrintIndentLevel :: Int
prettyPrintIndentLevel = 4

prettyPrintList :: (LispVal, Int) -> String
prettyPrintList (xs, depth) = case xs of
    (List ys) -> indent depth ++ "( " ++ concatMap iteration ys ++ ") "
      where iteration ts = prettyPrintList (ts, depth + 1)
            indent d = if d == 0 then "" else "\n" ++ concat
                (replicate (d * prettyPrintIndentLevel) " ")
    _         -> prettyPrint xs

prettyPrint :: LispVal -> String
prettyPrint (List xs) = prettyPrintList (List xs, 0)
prettyPrint (DottedList xs end) = "( " ++ concatMap prettyPrint xs ++ ". " ++
                                  prettyPrint end ++ ") "
prettyPrint (Atom x) = init $ tail $ show x ++ " "
prettyPrint (Number x) = show x ++ " "
prettyPrint (String x) = show x ++ " "
prettyPrint (Char x) = "#\\" ++ init (tail $ show x) ++ " "
prettyPrint (Bool x) = if x then "#t " else "#f "

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseStartingWithOctothorpe
         <|> parseString
         <|> parseFloat
         <|> parseDecimal
         <|> parseQuoted
         <|> parseListOrDottedList

readExpr :: String -> String
readExpr input = case parse (sepBy parseExpr space) "readExprRoot" input of
    Left err -> "No match: " ++ show err
    Right val -> unlines (map prettyPrint val)

spaces :: Parser ()
spaces = skipMany1 space

parseStartingWithOctothorpe :: Parser LispVal
parseStartingWithOctothorpe = char '#' >>
                              (parseBool
                           <|> parseChar
                           <|> (char 'd' >> parseDecimal)
                           <|> (char 'x' >> parseHex)
                           <|> (char 'b' >> parseBin)
                           <|> (char 'o' >> parseOct))

parseBool :: Parser LispVal
parseBool = Bool . (== 't') <$> oneOf "tf"

parseChar :: Parser LispVal
parseChar = do
    first <- char '\\' >> anyChar
    rest <- getRest first
    let whole = first:rest
    return $ Char $ if length whole == 1
                       then head whole
                       else case map toLower whole of
                              "space"   -> ' '
                              "newline" -> '\n'
                              "tab"     -> '\t'
                              _ -> error "Unrecognized char literal name"
    where
        getRest first = if isAlpha first
                           then many $ noneOf delimiter
                           else return ""

delimiter :: [Char]
delimiter = " \n()\";"

extendedAlphabeticChars:: [Char]
extendedAlphabeticChars = "+-.*/<=>!?:$%_&~^"

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> oneOf extendedAlphabeticChars
    rest <- many $ letter <|> oneOf extendedAlphabeticChars <|>
                   digit <|> oneOf ".+-"
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

parseDecimal :: Parser LispVal
parseDecimal = Number . read <$> many1 digit

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    _ <- char '.'
    y <- many1 digit
    return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseHex :: Parser LispVal
parseHex = (Number . hexToDec) <$>  many1 hexDigit
  where hexToDec x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = (Number . octToDec) <$>  many1 octDigit
  where octToDec x = fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = (Number . binToDec . map digitToInt) <$> many1 (oneOf "01")
  where binToDec x = sum $ map (2^) $ findIndices (==1) $ reverse x

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
