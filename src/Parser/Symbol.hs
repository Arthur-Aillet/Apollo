{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- Parse Symbol
-}

module Parser.Symbol (module Parser.Symbol) where

import Parser.Char (parseChar)
import Parser.Type (Parser (..))

parseSymbol :: String -> Parser String
parseSymbol str
  | length str == 1 = Parser $ \s p -> case runParser (parseChar (head str)) s p of
      Right (char, new_str, new_pos) -> Right ([char], new_str, new_pos)
      Left err -> Left err
parseSymbol (x : xs) = Parser $ \s p -> case runParser (parseChar x) s p of
  Left err -> Left err
  Right (new, new_str, new_pos) -> case runParser (parseSymbol xs) new_str new_pos of
    Left err -> Left err
    Right (found, fd_str, fd_pos) -> Right (new : found, fd_str, fd_pos)
