{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- Ast parser
-}

module Parser.Ast (module Parser.Ast) where
import Ast.Ast (Ast (..), Operation)
import Parser.Bool (parseBool)
import Parser.Type (Parser (..))
import Parser (Parser (Parser, runParser))
import {-# SOURCE #-} Parser.Structure (parseAstStructure)
import Parser.Operation (parseOperation)
import Control.Applicative (Alternative ((<|>)))
import Parser.Syntax (parseMaybeparenthesis)

getAstOperation :: Parser Operation -> Parser Ast
getAstOperation parser = Parser $ \s p -> case runParser parser s p of
  Right (operation, str, pos) -> Right (AstOperation operation, str, pos)
  Left a -> Left a

parseAst :: Parser Ast
parseAst =  getAstOperation (parseMaybeparenthesis parseOperation)
        <|> parseAstStructure

