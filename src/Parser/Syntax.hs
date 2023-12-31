{-
-- EPITECH PROJECT, 2023
-- Apollo
-- File description:
-- SyntaxParser
-}

module Parser.Syntax (module Parser.Syntax) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Char (parseAnyChar, parseChar, parseClosingParenthesis, parseOpeningParenthesis)
import Parser.Range (Range (..))
import Parser.StackTrace (SourceLocation (..), StackTrace (..))
import Parser.Type (Parser (..))

parseMany :: Parser a -> Parser [a]
parseMany parse = Parser $ \string p -> case runParser parse string p of
  Right (element, new_str, new_pos) ->
    case runParser (parseMany parse) new_str new_pos of
      Left _ -> Right ([], new_str, new_pos)
      Right (found, fd_str, fd_pos) -> Right (element : found, fd_str, fd_pos)
  Left _ -> Right ([], string, p)

parseSpaces :: Parser Char
parseSpaces = parseAnyChar [' ', '\n', '\t']

parseOnlySpaces :: Parser String
parseOnlySpaces = Parser $ \string p -> case string of
  [] -> Right ([], string, p)
  _ -> case runParser parseSpaces string p of
    Right (new, new_str, new_pos) ->
      case runParser parseOnlySpaces new_str new_pos of
        Left (StackTrace [(str, Range _ p2, src)]) ->
          Left (StackTrace [(str, Range p p2, src)])
        Left a -> Left a
        Right (found, fd_str, fd_pos) -> Right (new : found, fd_str, fd_pos)
    Left a -> Left a

parseManyValidOrEmpty :: Parser a -> Parser [a]
parseManyValidOrEmpty parse = Parser $ \st p -> case runParser parse st p of
  Right (element, new_str, new_pos) ->
    case runParser (parseManyValidOrEmpty parse) new_str new_pos of
      Left a -> case runParser parseOnlySpaces new_str new_pos of
        Left _ -> Left a
        Right (_, fd_str, fd_pos) -> Right ([element], fd_str, fd_pos)
      Right (found, fd_str, fd_pos) -> Right (element : found, fd_str, fd_pos)
  Left a -> Left a

----------------------------------------------------------------------

parseManyStruc :: [(String, Range, SourceLocation)] -> a -> Parser [a]
parseManyStruc err element = Parser $ \s p ->
  case runParser parseOnlySpaces s p of
    Left _ -> case runParser (parseWithSpace (parseChar '}')) s p of
      Right _ -> Right ([element], s, p)
      Left _ -> Left (StackTrace err)
    Right (_, fd_str, fd_pos) -> Right ([element], fd_str, fd_pos)

parseManyStructure :: Parser a -> Parser [a]
parseManyStructure parse = Parser $ \st p -> case runParser parse st p of
  Right (element, new_str, new_pos) ->
    case runParser (parseManyStructure parse) new_str new_pos of
      Left (StackTrace [("", _, _)]) -> Right ([], st, p)
      Left (StackTrace a) ->
        runParser (parseManyStruc a element) new_str new_pos
      Right (found, fd_str, fd_pos) -> Right (element : found, fd_str, fd_pos)
  Left (StackTrace [("", _, _)]) -> Right ([], st, p)
  Left a -> Left a

-----------------------------------------------------------------------

parseSome :: Parser a -> Parser [a]
parseSome parse = (:) <$> parse <*> parseMany parse

parseWithSpace :: Parser a -> Parser a
parseWithSpace parser =
  parseMany parseSpaces *> parser <* parseMany parseSpaces

parseMaybeparenthesis :: Parser a -> Parser a
parseMaybeparenthesis parser =
  parseWithSpace parser
    <|> parseWithSpace
      ( parseOpeningParenthesis
          *> parseWithSpace parser
          <* parseClosingParenthesis
      )
