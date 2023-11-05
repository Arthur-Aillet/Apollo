{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- String
-}

module Parser.String (module Parser.String) where

import Parser.Char (parseAnyChar, parseClosingQuote, parseOpeningQuote)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))

exInput :: String
exInput = "\"test\""

acceptableCharacters :: [Char]
acceptableCharacters =
  ['a' .. 'z']
    ++ ['A' .. 'Z']
    ++ ['0' .. '9']
    ++ ['|', '/', '\\', '[', ']', '(', ')', '{', '}', '-', '_', '\"', '\'']

parseStringContent :: Parser String
parseStringContent = parseMany $ parseAnyChar acceptableCharacters

parseBetweenQuotes :: Parser a -> Parser a
parseBetweenQuotes parser = parseOpeningQuote *> parser <* parseClosingQuote

parseString :: Parser String
parseString = parseWithSpace (parseBetweenQuotes parseStringContent)
