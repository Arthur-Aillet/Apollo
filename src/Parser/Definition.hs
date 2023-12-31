{-
-- EPITECH PROJECT, 2023
-- Apollo2
-- File description:
-- Definition.hs
-}

module Parser.Definition (module Parser.Definition) where

import Ast.Ast (Ast (..), Definition (..), Function (..), Type (..))
import Control.Applicative (Alternative ((<|>)))
import Data.Tuple (swap)
import Parser.Char (parseAChar, parseChar, parseClosingCurlyBraquet, parseClosingParenthesis, parseOpeningCurlyBraquet, parseOpeningParenthesis)
import Parser.Operable (parseDefinitionName)
import Parser.Position (Position)
import Parser.Range (Range (..))
import Parser.StackTrace (SourceLocation, StackTrace (..), addSourceLocation, modifySourceLocation)
import Parser.Structure (parseSequence)
import Parser.Symbol (parseMaybeType, parseType)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))

parseParameter :: Parser (String, Type)
parseParameter =
  swap <$> ((,) <$> typ <*> (parseChar ' ' *> str))
  where
    typ = parseType
    str = parseWithSpace parseDefinitionName

parseParameterWithComa :: Parser (String, Type)
parseParameterWithComa = parseParameter <* parseChar ','

parseParameters :: Parser [(String, Type)]
parseParameters =
  parseWithSpace
    ( parseOpeningParenthesis
        *> parseMany
          (parseWithSpace (parseParameterWithComa <|> parseParameter))
        <* parseClosingParenthesis
    )

parseInstruction :: Parser Ast
parseInstruction = AstStructure <$> parseSequence

parseInstructions :: Parser Ast
parseInstructions =
  parseWithSpace
    ( parseOpeningCurlyBraquet
        *> parseWithSpace parseInstruction
        <* parseClosingCurlyBraquet
    )

parseFunction :: Parser Function
parseFunction =
  Function <$> parseParameters <*> parseMaybeType <*> parseInstructions

parseFuncDefinition :: Parser Definition
parseFuncDefinition = Parser $ \s p ->
  case runParser (parseChar '@' *> parseDefinitionName) s p of
    Right (name, str, pos) -> case runParser parseFunction str pos of
      Right (func, string, position) ->
        Right (FuncDefinition name func, string, position)
      Left (StackTrace a) ->
        Left (StackTrace (modifySourceLocation (addSourceLocation name p) a))
    Left a -> Left a

findNextFunction :: Int -> Parser Char
findNextFunction 0 = Parser $ \s p -> Right ('}', s, p)
findNextFunction nb_brackets = Parser $ \s p -> case runParser parseAChar s p of
  Right ('{', str, pos) ->
    if nb_brackets == -1
      then runParser (findNextFunction 1) str pos
      else runParser (findNextFunction (nb_brackets + 1)) str pos
  Right ('}', str, pos) ->
    runParser (findNextFunction (nb_brackets - 1)) str pos
  Right (_, str, pos) -> runParser (findNextFunction nb_brackets) str pos
  Left (StackTrace [(_, ran, src)]) -> Left (StackTrace [("", ran, src)])
  Left a -> Left a

parseNextFuncDef :: Parser [Definition] -> String -> Position -> String -> Position -> Position -> SourceLocation -> Either StackTrace b
parseNextFuncDef parser s p xs p1 p2 src =
  case runParser (findNextFunction (-1) *> parseManyFuncDef parser) s p of
    Right _ ->
      Left (StackTrace [(xs, Range p1 p2, src)])
    Left (StackTrace [("", Range _ p3, _)]) ->
      Left (StackTrace [(xs, Range p1 p3, src)])
    Left (StackTrace [("Not Found: End of Input", ran, sr)]) ->
      Left (StackTrace [("", ran, sr)])
    Left (StackTrace ys) -> Left $ StackTrace $ (xs, Range p1 p2, src) : ys

parseManyFuncDef :: Parser [Definition] -> Parser [Definition]
parseManyFuncDef parser = Parser $ \s p ->
  case runParser (parseWithSpace parser) s p of
    Right a -> Right a
    Left (StackTrace [(xs, Range p1 p2, src)]) ->
      parseNextFuncDef parser s p xs p1 p2 src
    Left a -> Left a
