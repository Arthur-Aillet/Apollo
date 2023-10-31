{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Definition.hs
-}

module Parser.Definition (module Parser.Definition) where

import Ast.Type (Ast (..), Definition (..), Function (..), Operable (..), Structure (..), Type (..))
import Control.Applicative (Alternative ((<|>)))
import Data.Tuple (swap)
import Parser.Char (parseChar, parseClosingCurlyBraquet, parseClosingParenthesis, parseOpeningCurlyBraquet, parseOpeningParenthesis)
import Parser.Operable (parseDefinitionName, parseOperable)
import Parser.Symbol (parseMaybeType, parseSymbol, parseType)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))

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
    ( parseOpeningParenthesis
        *> parseMany (parseWithSpace (parseParameterWithComa <|> parseParameter))
        <* parseClosingParenthesis
    )

parseInstruction :: Parser Ast
parseInstruction = AstStructure <$> (returnVar <$> parseWithSpace (parseSymbol "return") <*> parseOperable)
  where
    returnVar :: String -> Operable -> Structure
    returnVar _ expr = Return expr

parseInstructions :: Parser Ast
parseInstructions =
  parseWithSpace
    ( parseOpeningCurlyBraquet
        *> parseWithSpace parseInstruction
        <* parseClosingCurlyBraquet
    )

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
