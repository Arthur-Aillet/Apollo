--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- String
--

module Parser.String (module Parser.String) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Char (parseChar, parseDigit, parseAnyChar, parseOpeningQuote, parseClosingQuote, parseOpeningsQuote, parseClosingsQuote)
import Parser.StackTrace (StackTrace (..))
import Parser.Type (Parser (..))
import Parser.Syntax (parseMany, parseSome, parseWithSpace)
import Parser.Range (Range (..))
import Control.Applicative (Alternative ((<|>)))

exInput :: String
exInput = "\"test\""

acceptableCharacters :: [Char]
acceptableCharacters = ['a'..'z']
                    ++ ['A'..'Z']
                    ++ ['0'..'9']
                    ++ ['|', '/', '\\', '[', ']', '(', ')', '{', '}', '-', '_', '\"', '\'']

parseStringContent :: Parser String
parseStringContent = parseMany $ parseAnyChar acceptableCharacters

parseBetweenQuotes :: Parser a -> Parser a
parseBetweenQuotes parser = parseOpeningQuote *> parser <* parseClosingQuote

parseString :: Parser String
parseString = parseWithSpace (parseBetweenQuotes parseStringContent)