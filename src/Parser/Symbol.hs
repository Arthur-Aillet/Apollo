{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- Parse Symbol
-}

module Parser.Symbol (module Parser.Symbol) where

import Parser.Char (parseAnyChar)
import Parser.Range (Range (..), newRange)
import Parser.StackTrace (StackTrace (..))
import Parser.Syntax (parseSome)
import Parser.Type (Parser (..))

parseSymbol :: String -> Parser String
parseSymbol string = Parser $ \s p ->
  case runParser (parseSome (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z']))) s p of
    Right (found, n_s, n_p)
      | found == string -> Right (found, n_s, n_p)
      | otherwise -> Left StackTrace {errors = [("Error: Symbols are not the same", newRange p n_p)]}
    Left (StackTrace ((_, old_range) : xs)) -> Left StackTrace {errors = ("Not Found: List is empty", newRange p (end old_range)) : xs}
    Left _ -> Left StackTrace {errors = [("Not Found: List is empty", newRange p p)]}
