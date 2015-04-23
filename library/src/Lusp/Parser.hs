module Lusp.Parser (parse
                   ,parseString) where

import Lusp.LispError (LispError(ParseError))
import Lusp.LispVal (LispVal(List
                            ,DottedList
                            ,Atom
                            ,Vector
                            ,Integer
                            ,Real
                            ,Ratio
                            ,Complex
                            ,String
                            ,Char
                            ,Bool))

import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P (parse)

import Control.Exception (throw)
import Data.Char (toLower, digitToInt, isAlpha)
import Data.Complex (Complex((:+)))
import Data.List (elemIndices)
import Data.Ratio ((%))
import Numeric (readFloat, readHex, readOct)

-- | Parse a single expression
parseExpr :: Parser LispVal
parseExpr = ignored
         *> (parseAtom
         <|> parseStartingWithOctothorpe
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> parseListOrDottedList)
         <*  ignored

-- | Parse any number of expressions.
-- May throw a 'Lusp.LispError.ParseError'
parse :: String -> [LispVal]
parse input =
    case P.parse ((ignored *> many parseExpr) <|> ignoreEOF) "" input of
      Left e -> throw $ ParseError e
      Right val -> val
  where ignoreEOF = ignored >> return []

-- | Parse and ignore whitespace and comments
ignored :: Parser ()
ignored = spaces *> skipMany (comment *> spaces) *> spaces

parseStartingWithOctothorpe :: Parser LispVal
parseStartingWithOctothorpe =  char '#' *>
                              (parseBool
                           <|> parseChar
                           <|> parseVector
                           <|> char 'd' *> parseNumber
                           <|> char 'x' *> parseHex
                           <|> char 'b' *> parseBin
                           <|> char 'o' *> parseOct)

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
        delimiter = " \n()\";"
        getRest :: Char -> Parser String
        getRest first = if isAlpha first
                           then many $ noneOf delimiter
                           else return ""

parseAtom :: Parser LispVal
parseAtom = peculiar >>= (\x -> case x of
                         Just p  -> return $ Atom p
                         Nothing -> Atom <$> ((:) <$> initial <*> subsequent))
      where initial = letter <|> specialInitial
            specialInitial = oneOf "!$%&*/:<=>?^_~"
            subsequent = many $ initial <|> digit <|> specialSubsequent
            specialSubsequent = oneOf "+-.@"
            peculiarIdentifier = choice $ map string ["+", "-", "..."]
            peculiar = try $ optionMaybe peculiarIdentifier

parseNumber :: Parser LispVal
parseNumber =  try parseRatio
           <|> try parseComplex
           <|> try parseReal
           <|> parseInteger

parseInteger :: Parser LispVal
parseInteger = (Integer . read) <$> many1 digit

parseReal :: Parser LispVal
parseReal = do x <- many1 digit <* char '.'
               y <- many1 digit
               return . Real . fst . head $ readFloat (x ++ "." ++ y)

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit <* char '/'
                y <- many1 digit
                return $ Ratio (read x % read y)

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseReal <|> parseInteger) <* char '+'
                  y <- (try parseReal <|> parseInteger) <* char 'i'
                  return $ Complex (toDouble x :+ toDouble y)
                    where toDouble :: LispVal -> Double
                          toDouble (Real f)    = f
                          toDouble (Integer n) = fromIntegral n
                          toDouble _           = error "can't convert to double"

parseHex :: Parser LispVal
parseHex = (Integer . hexToDec) <$> many1 hexDigit
  where hexToDec = fst . head . readHex

parseOct :: Parser LispVal
parseOct = (Integer . octToDec) <$> many1 octDigit
  where octToDec = fst . head . readOct

parseBin :: Parser LispVal
parseBin = (Integer . binToDec . map digitToInt) <$> many1 (oneOf "01")
  where binToDec = sum . map (2^) . elemIndices 1 . reverse

parseString :: Parser LispVal
parseString = String <$> between (char '"') (char '"') (many chars)
  where chars = escaped <|> noneOf "\""
        escaped = char '\\' >> choice (zipWith escChar characters replacements)
        escChar :: Char -> Char -> Parser Char
        escChar character replacement = char character >> return replacement
        characters   = [ 'b',  'n',  'f',  'r',  't', '\\', '\"', '/']
        replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseList :: Parser [LispVal]
parseList = many parseExpr

parseListOrDottedList :: Parser LispVal
parseListOrDottedList = do
    list <- char '(' *> parseList
    end <- optionMaybe (char '.' *> parseExpr) <* char ')'
    return $ case end of
               Nothing -> List list
               Just x -> DottedList list x

parseVector :: Parser LispVal
parseVector = Vector <$> (char '(' *> parseList <* char ')')

parseQuoted :: Parser LispVal
parseQuoted = (\x -> List [Atom "quote", x]) <$> (char '\'' *> parseExpr)

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = (\x -> List [Atom "quasiquote", x]) <$>
                   (char '`' *> parseExpr)

parseUnQuote :: Parser LispVal
parseUnQuote = (\x -> List [Atom "unquote", x]) <$> (char ',' *> parseExpr)

comment :: Parser ()
comment = char ';' *>
    manyTill anyChar ((newline *> return ()) <|> eof) *> return ()
