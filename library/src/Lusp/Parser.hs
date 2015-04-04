module Lusp.Parser (parseExpressions) where

import Lusp.LispError (LispError(..))
import Lusp.LispVal (LispVal(..))

import Text.Parsec.Token (float)
import Text.ParserCombinators.Parsec

import Control.Applicative ((<$>), (<*), (*>), (<*>))
import Control.Exception (throw)
import Data.Char (toLower, digitToInt, isAlpha)
import Data.Complex (Complex((:+)))
import Data.List (findIndices)
import Data.Ratio ((%))
import Numeric (readFloat, readHex, readOct)

parseExpr :: Parser LispVal
parseExpr =  spaces *>
            (parseAtom
         <|> parseStartingWithOctothorpe
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> parseListOrDottedList)
         <*  spaces

parseExpressions :: String -> [LispVal]
parseExpressions input = case parse (many parseExpr) "" input of
                           Left e -> throw $ Parser e
                           Right val -> val

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
        getRest :: Char -> Parser String
        getRest first = if isAlpha first
                           then many $ noneOf delimiter
                           else return ""

delimiter :: [Char]
delimiter = " \n()\";"

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
           <|> try parseFloat
           <|> parseInteger

parseInteger :: Parser LispVal
parseInteger = (Integer . read) <$> many1 digit

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit <* char '.'
                y <- many1 digit
                return . Float . fst . head $ readFloat (x ++ "." ++ y)

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit <* char '/'
                y <- many1 digit
                return $ Ratio (read x % read y)

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseInteger) <* char '+'
                  y <- (try parseFloat <|> parseInteger) <* char 'i'
                  return $ Complex (toFloat x :+ toFloat y)
                    where toFloat :: LispVal -> Float
                          toFloat (Float f)   = f
                          toFloat (Integer n) = fromIntegral n

parseHex :: Parser LispVal
parseHex = (Integer . hexToDec) <$> many1 hexDigit
  where hexToDec x = fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = (Integer . octToDec) <$> many1 octDigit
  where octToDec x = fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = (Integer . binToDec . map digitToInt) <$> many1 (oneOf "01")
  where binToDec x = sum $ map (2^) $ findIndices (==1) $ reverse x

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
