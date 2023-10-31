{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Structure.hs
-}

module Parser.Structure (module Parser.Structure) where

import Ast.Type (Ast (..), Operable (..), Structure (..), Type (..))
import Control.Applicative (Alternative ((<|>)))
import Parser.Char (parseAChar, parseAnyChar, parseChar)
import Parser.Operable (parseDefinitionName, parseOperable)
import Parser.Symbol (parseSymbol, parseType)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))

----------------------------------------------------------------

parseAstStructure :: Parser Ast
parseAstStructure = AstStructure <$> (parseVarAssignation <|> parseVarDefinition <|> parseReturn <|> parseSequence)

----------------------------------------------------------------

acceptableCharacters :: [Char]
acceptableCharacters =
  ['a' .. 'z']
    ++ ['A' .. 'Z']
    ++ ['0' .. '9']
    ++ ['|', '/', '[', ']', '(', ')', '{', '}', '-', '_', '"', '\'']
    ++ [' ', '+', '?', '.', ':', '!', ';', '\\']

parseStringWithHandleBackslash :: Parser String
parseStringWithHandleBackslash = parseMany (((parseChar '\\') *> (parseChar '\\')) <|> ((parseChar '\\') *> parseAChar) <|> parseAChar)

----------------------------------------------------------------

-- FIXME - VarDefinition (Maybe Operable)
createVarDef :: Parser Type -> Parser String -> Parser (Maybe Operable) -> Parser Structure
createVarDef parType parStr op = Parser $ \s p -> case runParser parType s p of
  Right (typ, str, pos) -> case runParser parStr str pos of
    Right (name, string, position) -> case runParser op string position of
      Right (ope, new_str, new_pos) -> Right ((VarDefinition name typ ope), new_str, new_pos)
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
parseSingle :: Parser Structure
parseSingle = Single <$> parseAstStructure

parseSequence :: Parser Structure
parseSequence = Sequence <$> (parseMany parseAstStructure)

----------------------------------------------------------------
