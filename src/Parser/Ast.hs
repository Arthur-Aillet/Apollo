{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- Ast parser
-}

module Parser.Ast (module Parser.Ast) where
import Ast.Type (Ast (AstOperation), Operation)
import Parser (Parser (Parser, runParser))
import {-# SOURCE #-} Parser.Structure (parseAstStructure)
import Parser.Operation (parseOperation)
import Control.Applicative (Alternative ((<|>)))

getAstOperation :: Parser Operation -> Parser Ast
getAstOperation parser = Parser $ \s p -> case runParser parser s p of
  Right (operation, str, pos) -> Right (AstOperation operation, str, pos)
  Left a -> Left a

parseAst :: Parser Ast
parseAst =  getAstOperation parseOperation
        <|> parseAstStructure