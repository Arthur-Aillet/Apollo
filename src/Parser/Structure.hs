{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Structure.hs
-}

module Parser.Structure (module Parser.Structure) where

import Parser.Type (Parser(..))
import Ast.Type(Structure(..), Type(..), Operable(..))
import Parser.Symbol(parseType)
import Parser.Operable(parseDefinitionName, parseOperable)
import Parser.Syntax(parseWithSpace)
import Parser.Char(parseChar)
import Parser.Symbol(parseSymbol)

----------------------------------------------------------------

createVarDef :: Parser Type -> Parser String -> (Maybe Operable) -> Parser Structure
createVarDef  parType parStr op = Parser $ \s p -> case runParser parType s p of
  Right(typ, str, pos) -> case runParser parStr str pos of
    Right(name, string, position) -> Right ((VarDefinition name typ op), string, position)
    Left a -> Left a
  Left a -> Left a

parseVarDefinition :: Parser Structure
parseVarDefinition = createVarDef parseType parseDefinitionName Nothing

----------------------------------------------------------------

parseVarAssignation :: Parser Structure
parseVarAssignation = VarAssignation <$> parseWithSpace parseDefinitionName <*> (parseWithSpace (parseChar '=') *> parseOperable)

----------------------------------------------------------------

parseReturn :: Parser Structure
parseReturn = Return <$> ((parseWithSpace (parseSymbol "return")) *> parseOperable)

