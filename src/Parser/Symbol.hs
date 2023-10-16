{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- Parse Symbol
-}

module Parser.Symbol (module Parser.Symbol) where

import Parser.Char (parseAnyChar)
import Parser.Type (Parser (..))
import Parser.Syntax (parseSome)

parseSymbol :: String -> Parser String
parseSymbol string = Parser $ \s p ->
  case runParser (parseSome (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z']))) s p of
    Right (found, n_s, n_p)
      | found == string -> Right (found, n_s, n_p)
      | otherwise -> Left ("Error: Symbols are not the same", p)
    Left (_, new_pos) -> Left ("Not Found: List is empty", new_pos)
