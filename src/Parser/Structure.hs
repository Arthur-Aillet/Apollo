{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Structure.hs
-}

module Parser.Structure (module Parser.Structure) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Type (Parser(..))
import Ast.Type(Structure(..), Type(..), Operable(..), Ast(..))
import Parser.Symbol(parseType, parseSymbol)
import Parser.Operable(parseDefinitionName, parseOperable)
import Parser.Syntax(parseWithSpace, parseMany)
import Parser.Char(parseChar)

----------------------------------------------------------------

parseAstStructure :: Parser Ast
parseAstStructure = AstStructure <$> (parseVarAssignation <|> parseVarDefinition <|> parseReturn)

----------------------------------------------------------------
-- FIXME - VarDefinition (Maybe Operable)
createVarDef :: Parser Type -> Parser String -> Parser (Maybe Operable) -> Parser Structure
createVarDef  parType parStr op = Parser $ \s p -> case runParser parType s p of
  Right(typ, str, pos) -> case runParser parStr str pos of
    Right(name, string, position) -> case runParser op string position of
        Right(ope, new_str, new_pos) -> Right((VarDefinition name typ ope), new_str, new_pos)
        Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseVarDefinition :: Parser Structure
parseVarDefinition = createVarDef parseType parseDefinitionName (Just <$> (parseWithSpace (parseChar '=') *> parseOperable) <|> pure Nothing)

----------------------------------------------------------------

parseVarAssignation :: Parser Structure
parseVarAssignation = VarAssignation <$> parseWithSpace parseDefinitionName <*> (parseWithSpace (parseChar '=') *> parseOperable)

----------------------------------------------------------------

parseReturn :: Parser Structure
parseReturn = Return <$> ((parseWithSpace (parseSymbol "return")) *> parseOperable)

----------------------------------------------------------------

-- FIXME - Change parseAstStructure by parseAst
parseSequence :: Parser Structure
parseSequence = Sequence <$> (parseMany parseAstStructure)
