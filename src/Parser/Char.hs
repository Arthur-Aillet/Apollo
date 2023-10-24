{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseChar
-}

module Parser.Char (module Parser.Char) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Error (failingWith, withErr)
import Parser.Position (moveCursor)
import Parser.Type (Parser (..))

parseAChar :: Parser Char
parseAChar = Parser $ \string pos -> case string of
  ('\n' : xs) -> Right ('\n', xs, moveCursor pos True)
  (x : xs) -> Right (x, xs, moveCursor pos False)
  [] -> Left ("Not Found: List is empty", pos)

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0' .. '9']

parseOpeningQuote :: Parser Char
parseOpeningQuote = withErr "Not Found: Missing opening Quote" (parseChar '"')

parseClosingQuote :: Parser Char
parseClosingQuote = withErr "Not Found: Missing closing Quote" (parseChar '"')

parseOpeningParenthesis :: Parser Char
parseOpeningParenthesis =
  withErr "Not Found: Missing opening Parenthesis" (parseChar '(')

parseClosingParenthesis :: Parser Char
parseClosingParenthesis =
  withErr "Not Found: Missing closing Parenthesis" (parseChar ')')

parseChar :: Char -> Parser Char
parseChar x = Parser $ \string pos -> case runParser parseAChar string pos of
  Right (char, new_str, new_pos)
    | x == char -> Right (char, new_str, new_pos)
    | otherwise ->
        Left (err, moveCursor pos False)
    where
      err = "Not Found: charactere is not '" ++ [x] ++ "'"
  Left (_, new_pos) -> Left ("Not Found: List is empty", new_pos)

parseNotChar :: Char -> Parser Char
parseNotChar x = Parser $ \string pos -> case runParser parseAChar string pos of
  Right (char, new_str, new_pos)
    | x == char ->
        Left
          ( ("Not Found: charactere is not '" ++ [x] ++ "'"),
            moveCursor pos False
          )
    | otherwise -> Right (char, new_str, new_pos)
  Left (_, new_pos) -> Left ("Not Found: List is empty", new_pos)

parseAnyChar :: [Char] -> Parser Char
parseAnyChar =
  foldl
    (\a b -> a <|> parseChar b)
    (failingWith "Not Found: List is empty")
