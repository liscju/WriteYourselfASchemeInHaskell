module Main where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    deriving(Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString =
    do
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return $ String x


parseAtom :: Parser LispVal
parseAtom =
    do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _ -> Atom atom

octalPrefix :: Parser String
octalPrefix =
    try $ char '#' >> char 'b' >> return "#b"

hexPrefix :: Parser String
hexPrefix =
    try $ char '#' >> char 'h' >> return "#h"

parseNumber :: Parser LispVal
parseNumber =
    try $
         do
            parsedNum <- many1 (digit <|> letter)
            prefixType <- optionMaybe (hexPrefix <|> octalPrefix)
            let num = case prefixType of
                        Just "#b" -> fst $ head $ readOct parsedNum
                        Just "#h" -> fst $ head $ readHex parsedNum
                        Nothing -> read parsedNum
            return (Number num)

parseCharacter :: Parser LispVal
parseCharacter =
    try $
        do
            char '#'
            char '\\'
            character <- anyChar
            return (Character character)


parseExpr :: Parser LispVal
parseExpr = parseCharacter
    <|> parseNumber
    <|> parseAtom
    <|> parseString

readExpr :: String -> String
readExpr input =
    case parse parseExpr "lisp" input of
        Left err -> "No match: " ++ show err
        Right val -> "Found value: " ++ show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))













