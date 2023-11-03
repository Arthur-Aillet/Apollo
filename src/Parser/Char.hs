{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseChar
-}

module Parser.Char (module Parser.Char) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Error (failingWith, replaceErr)
import Parser.Position (moveCursor)
import Parser.Range (Range (..))
import Parser.StackTrace (defaultLocation)
import Parser.Type (Parser (..), StackTrace (..))

parseAChar :: Parser Char
parseAChar = Parser $ \string pos -> case string of
  ('\n' : xs) -> Right ('\n', xs, moveCursor pos True)
  (x : xs) -> Right (x, xs, moveCursor pos False)
  [] -> Left (StackTrace [("Not Found: End of Input", Range pos pos, defaultLocation)])

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0' .. '9']

---------------------------------------------

parseOpeningQuote :: Parser Char
parseOpeningQuote = replaceErr "Not Found: Missing opening Quote" (parseChar '"')

parseClosingQuote :: Parser Char
parseClosingQuote = replaceErr "Not Found: Missing closing Quote" (parseChar '"')

parseOpeningParenthesis :: Parser Char
parseOpeningParenthesis =
  replaceErr "Not Found: Missing opening Parenthesis" (parseChar '(')

parseClosingParenthesis :: Parser Char
parseClosingParenthesis =
  replaceErr "Not Found: Missing closing Parenthesis" (parseChar ')')

parseOpeningCurlyBraquet :: Parser Char
parseOpeningCurlyBraquet =
  replaceErr "Not Found: Missing opening curlybraquet" (parseChar '{')

parseClosingCurlyBraquet :: Parser Char
parseClosingCurlyBraquet =
  replaceErr "Not Found: Missing closing curlybraquet" (parseChar '}')

parseOpeningBraquet :: Parser Char
parseOpeningBraquet =
  replaceErr "parseOpeningBraquet: Not Found: Missing opening braquet" (parseChar '[')

parseClosingBraquet :: Parser Char
parseClosingBraquet =
  replaceErr "parseClosingBraquet: Not Found: Missing closing braquet" (parseChar ']')

---------------------------------------------

parseChar :: Char -> Parser Char
parseChar x = Parser $ \string pos -> case runParser parseAChar string pos of
  Right (char, new_str, new_pos)
    | x == char -> Right (char, new_str, new_pos)
    | otherwise ->
        Left (StackTrace [(err, err_range, defaultLocation)])
    where
      err = "Not Found: charactere is not '" ++ [x] ++ "' (is " ++ show char ++ ")"
      err_range = Range pos pos
  Left err -> Left err

parseNotChar :: Char -> Parser Char
parseNotChar x = Parser $ \string pos -> case runParser parseAChar string pos of
  Right (char, new_str, new_pos)
    | x == char -> Left (StackTrace [(err, err_range, defaultLocation)])
    | otherwise -> Right (char, new_str, new_pos)
    where
      err = "Not Found: character is '" ++ [x] ++ "'"
      err_range = Range pos pos
  Left err -> Left err

parseAnyChar :: [Char] -> Parser Char
parseAnyChar =
  foldl
    (\a b -> a <|> parseChar b)
    (failingWith "Not Found: List is empty")
