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
import Parser.Char(parseChar, parseAChar, parseOpeningParenthesis, parseClosingParenthesis)
import Parser.Error(replaceErr)

----------------------------------------------------------------

parseAstStructure :: Parser Ast
parseAstStructure = AstStructure <$> (parseVarAssignation <|> parseVarDefinition <|> parseReturn)

----------------------------------------------------------------

acceptableCharacters :: [Char]
acceptableCharacters = ['a'..'z']
                    ++ ['A'..'Z']
                    ++ ['0'..'9']
                    ++ ['|', '/', '[', ']', '(', ')', '{', '}', '-', '_', '"', '\'']
                    ++ [' ', '+', '?', '.', ':', '!', ';', '\\']

parseStringWithHandleBackslash :: Parser String
parseStringWithHandleBackslash =
  replaceErr "Syntaxe error: bad return"
  (parseMany (((parseChar '\\') *> (parseChar '\\')) <|> ((parseChar '\\') *> parseAChar) <|> parseAChar))

----------------------------------------------------------------

createVarDef :: Parser Type -> Parser String -> Parser (Maybe Operable) -> Parser Structure
createVarDef  parType parStr op = Parser $ \s p -> case runParser parType s p of
  Right(typ, str, pos) -> case runParser parStr str pos of
    Right(name, string, position) -> case runParser op string position of
        Right(ope, new_str, new_pos) -> Right((VarDefinition name typ ope), new_str, new_pos)
        Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseVarDefinition :: Parser Structure
parseVarDefinition = 
  replaceErr "Syntaxe error: bad variable definition"
  ((createVarDef parseType parseDefinitionName (Just <$> (parseWithSpace (parseChar '=') *> parseOperable) <|> pure Nothing)) <* parseChar ';')

----------------------------------------------------------------

parseVarAssignation :: Parser Structure
parseVarAssignation =
  replaceErr "Syntaxe error: bad assignment"
  (VarAssignation <$> parseWithSpace parseDefinitionName <*> (parseWithSpace (parseChar '=') *> parseOperable <* parseChar ';'))

----------------------------------------------------------------

parseReturnWithParenthesis :: Parser Operable
parseReturnWithParenthesis = parseWithSpace (parseSymbol "return") *> parseOpeningParenthesis *> parseOperable <* parseWithSpace parseClosingParenthesis

parseReturnWithoutParenthesis :: Parser Operable
parseReturnWithoutParenthesis = parseWithSpace (parseSymbol "return") *> parseOperable

parseReturn :: Parser Structure
parseReturn =
  replaceErr "Syntaxe error: bad return"
  (Return <$> ((parseReturnWithParenthesis <|> parseReturnWithoutParenthesis) <* parseChar ';'))

----------------------------------------------------------------

-- FIXME - Change parseAstStructure by parseAst
parseSingle :: Parser Structure
parseSingle = Single <$> parseAstStructure

parseSequence :: Parser Structure
parseSequence = Sequence <$> (parseMany parseAstStructure)

----------------------------------------------------------------


