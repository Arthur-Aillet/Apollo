{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- SyntaxParser
-}

module Parser.Syntax (module Parser.Syntax) where

import Parser.Type (Parser (..))
import Parser.Char (parseAnyChar)

parseMany :: Parser a -> Parser [a]
parseMany parse = Parser $ \string pos -> case runParser parse string pos of
  Right (element, new_str, new_pos) ->
    case runParser (parseMany parse) new_str new_pos of
      Left _ -> Right ([], new_str, new_pos)
      Right (found, fd_str, fd_pos) -> Right (element : found, fd_str, fd_pos)
  Left _ -> Right ([], string, pos)

parseSpaces :: Parser Char
parseSpaces = parseAnyChar [' ', '\n', '\t']

parseOnlySpaces :: Parser String
parseOnlySpaces = Parser $ \string pos -> case string of
  [] -> Right ([], string, pos)
  _ -> case runParser parseSpaces string pos of
    Right (new, new_str, new_pos) ->
      case runParser parseOnlySpaces new_str new_pos of
        Left a -> Left a
        Right (found, fd_str, fd_pos) -> Right (new : found, fd_str, fd_pos)
    Left a -> Left a

parseManyValidOrEmpty :: Parser a -> Parser [a]
parseManyValidOrEmpty parse = Parser $ \st pos -> case runParser parse st pos of
  Right (element, new_str, new_pos) ->
    case runParser (parseManyValidOrEmpty parse) new_str new_pos of
      Left a -> case runParser parseOnlySpaces new_str new_pos of
        Left _ -> Left a
        Right (_, fd_str, fd_pos) -> Right ([element], fd_str, fd_pos)
      Right (found, fd_str, fd_pos) -> Right (element : found, fd_str, fd_pos)
  Left a -> Left a

parseSome :: Parser a -> Parser [a]
parseSome parse = (:) <$> parse <*> parseMany parse

parseWithSpace :: Parser a -> Parser a
parseWithSpace parser =
  parseMany parseSpaces *> parser <* parseMany parseSpaces
