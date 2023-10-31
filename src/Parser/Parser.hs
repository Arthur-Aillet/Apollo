{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Parser.hs
-}

module Parser.Parser (parser) where

import Parser.Definition(parseFuncDefinition)
import Parser.Syntax(parseMany, parseWithSpace)
import Parser.Type(Parser(..))
import Parser.Position(Position(..), defaultPosition)
import Ast.Type (Definition(..))

parser :: String -> [Definition]
parser str = case runParser (parseMany parseFuncDefinition) str defaultPosition of
    Right (def, str, pos) -> def
    Left a -> []

