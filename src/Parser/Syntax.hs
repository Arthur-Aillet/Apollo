{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- SyntaxParser
-}

module Parser.Syntax (module Parser.Syntax) where

import Parser.Char (parseAnyChar)
import Parser.Type (Parser (..))
import Control.Applicative (Alternative ((<|>)))
-- import Parser.StackTrace (StackTrace(..))
import Parser.Char(parseNotChar, parseChar, parseOpeningParenthesis, parseClosingParenthesis)

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

-- parseManyFunc :: Parser a -> Parser [a]
-- parseManyFunc parse = Parser $ \s p -> case runParser parse s p of
--   Right (element, new_str, new_pos) -> case runParser (parseManyFunc parse) new_str new_pos of
--     Right (found, st, ps) -> Right (element : found, st, ps)

findNewInstruction :: Parser Char
findNewInstruction = parseMany (parseNotChar ';') *> parseChar ';'

-- parseManyInstruction :: Parser a -> Parser [a]
-- parseManyInstruction parser =  Parser $ \s p -> case runParser parser s p of
--   Right (elem, new_str, new_pos) -> case runParser (parseManyInstruction parser) new_str new_pos of
--     Right (found, st, ps) -> Right (elem : found, st, ps)
--     Left (StackTrace xs) -> case runParser (findNewInstruction *> parseManyInstruction parser) s p of
--       Right _ -> Left (StackTrace xs)
--       Left (StackTrace ys) -> if xs == ys then Left (StackTrace xs) else Left (StackTrace (xs ++ ys))
--   Left a -> Left a

parseSome :: Parser a -> Parser [a]
parseSome parse = (:) <$> parse <*> parseMany parse

parseWithSpace :: Parser a -> Parser a
parseWithSpace parser =
  parseMany parseSpaces *> parser <* parseMany parseSpaces

parseMaybeparenthesis :: Parser a -> Parser a
parseMaybeparenthesis parser =  parseWithSpace parser
                            <|> parseWithSpace (parseOpeningParenthesis *> parseWithSpace parser <* parseClosingParenthesis)
