import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Applicative ((<$>), (<*), (*>))
import Data.Char
import Numeric
import Text.Parsec.Token (float)
import Data.List (findIndices)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
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
prettyPrint (Atom x) = (init $ tail $ show x) ++ " "
prettyPrint (Integer x) = show x ++ " "
prettyPrint (Float x) = show x ++ " "
prettyPrint (String x) = show x ++ " "
prettyPrint (Char x) = "#\\" ++ init (tail $ show x) ++ " "
prettyPrint (Bool x) = if x then "#t " else "#f "

parseExpr :: Parser LispVal
parseExpr = many spaces
            *>  (parseAtom
            <|> parseStartingWithOctothorpe
            <|> parseString
            <|> parseDecimal
            <|> parseQuoted
            <|> parseListOrDottedList)
            <*  many spaces

readExpr :: String -> String
readExpr input = case parse (many parseExpr) "readExprRoot" input of
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
parseDecimal = do
    front <- many1 digit
    back <- optionMaybe (char '.' >> many1 digit)
    return $ case back of
               Just b -> Float . fst . head $ readFloat (front ++ "." ++ b)
               Nothing -> (Integer . read) front

parseHex :: Parser LispVal
parseHex = (Integer . hexToDec) <$>  many1 hexDigit
  where hexToDec x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = (Integer . octToDec) <$>  many1 octDigit
  where octToDec x = fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = (Integer . binToDec . map digitToInt) <$> many1 (oneOf "01")
  where binToDec x = sum $ map (2^) $ findIndices (==1) $ reverse x

parseString :: Parser LispVal
parseString = String <$> between (char '"') (char '"') (many chars)
  where chars = escaped <|> noneOf "\""
        escaped = char '\\' >> choice (zipWith escChar characters replacements)
        escChar character replacement = char character >> return replacement
        characters   = [ 'b',  'n',  'f',  'r',  't', '\\', '\"', '/']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseList :: Parser [LispVal]
parseList = many parseExpr

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
    list <- char '(' *> parseList
    end <- optionMaybe (char '.' >> parseExpr) <* char ')'
    return $ case end of
               Nothing -> List list
               Just x -> DottedList list x

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >>
              parseExpr >>= \ x ->
              return $ List [Atom "quote", x]
