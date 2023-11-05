{-
-- EPITECH PROJECT, 2023
-- Apollo
-- File description:
-- Ast parser
-}

module Parser.Ast (module Parser.Ast) where

import Ast.Ast (Ast (..), Operation)
import Control.Applicative (Alternative ((<|>)))
import Parser.Operation (parseOperation)
import {-# SOURCE #-} Parser.Structure (parseAstStructure)
import Parser.Syntax (parseMaybeparenthesis)
import Parser.Type (Parser (..))

getAstOperation :: Parser Operation -> Parser Ast
getAstOperation parser = Parser $ \s p -> case runParser parser s p of
  Right (operation, str, pos) -> Right (AstOperation operation, str, pos)
  Left a -> Left a

parseAst :: Parser Ast
parseAst =
  getAstOperation (parseMaybeparenthesis parseOperation) <* parseChar ';'
    <|> parseAstStructure
