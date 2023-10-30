{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Definition.hs
-}

module Parser.Definition (module Parser.Definition) where

import Data.Tuple (swap)
import Control.Applicative (Alternative ((<|>)))
import Parser.Type (Parser(..))
import Ast.Type(Ast (..), Function (..), Structure (..), Type (..), Definition (..), Operable (..))
import Parser.Symbol (parseSymbol, parseType, parseMaybeType)
import Parser.Syntax(parseMany, parseWithSpace)
import Parser.Char(parseChar, parseOpeningParenthesis, parseClosingParenthesis, parseOpeningCurlyBraquet, parseClosingCurlyBraquet)
import Parser.Operable(parseOperable, parseDefinitionName)

createVarDef :: Parser Type -> Parser String -> (Maybe Operable) -> Parser Structure
createVarDef  parType parStr op = Parser $ \s p -> case runParser parType s p of
  Right(typ, str, pos) -> case runParser parStr str pos of
    Right(name, string, position) -> Right ((VarDefinition name typ op), string, position)
    Left a -> Left a
  Left a -> Left a

parseDeclareVar :: Parser Structure
parseDeclareVar = createVarDef parseType parseDefinitionName Nothing

parseParameter :: Parser (String, Type)
parseParameter =
  (swap <$> ((,) <$> typ <*> str))
    where
      typ = parseType
      str = parseDefinitionName

parseParameterWithComa :: Parser (String, Type)
parseParameterWithComa = parseParameter <* parseChar ','

parseParameters :: Parser [(String, Type)]
parseParameters =
  parseWithSpace
  (parseOpeningParenthesis
  *>  parseMany (parseWithSpace (parseParameterWithComa <|> parseParameter))
  <* parseClosingParenthesis)

parseInstruction :: Parser Ast
parseInstruction = AstStructure <$> (returnVar <$> parseWithSpace (parseSymbol "return") <*> parseOperable)
  where
    returnVar :: String -> Operable -> Structure
    returnVar _ expr = Return expr

parseInstructions :: Parser Ast
parseInstructions =
  parseWithSpace
  (parseOpeningCurlyBraquet
  *> parseWithSpace parseInstruction
  <* parseClosingCurlyBraquet)

parseFunction :: Maybe Type -> Parser Function
parseFunction typ = Function <$> parseParameters <*> pure typ <*> parseInstructions

parseFuncDefinition :: Parser Definition
parseFuncDefinition = Parser $ \s p -> case runParser parseMaybeType s p of
  Right (typ, str, pos) -> case runParser parseDefinitionName str pos of
    Right (name, string, position) -> case runParser (parseFunction typ) string position of
      Right (func, new_str, new_pos) -> Right ((FuncDefinition name func), new_str, new_pos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a