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
import Ast.Type(Ast (..), Function (..), Structure (..), Operation (..), Type (..), Definition (..), Operable (..))
import Parser.Symbol (parseSymbol)
import Parser.StackTrace (StackTrace(..), defaultLocation)
import Parser.Range (Range(..))
import Parser.Syntax(parseMany, parseWithSpace, parseSome)
import Parser.Char(parseAnyChar, parseChar, parseOpeningParenthesis, parseClosingParenthesis, parseOpeningCurlyBraquet, parseClosingCurlyBraquet)
import Parser.Position(Position(..))

parseDefinitionName :: Parser String
parseDefinitionName = parseWithSpace (parseMany (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_-")))

goodType :: String -> Type
goodType "int" = TypeInt
goodType "float" = TypeFloat
goodType "char" = TypeChar
goodType "bool" = TypeBool

parseType :: Parser String
parseType = parseSymbol "int" <|> parseSymbol "float" <|> parseSymbol "bool" <|> parseSymbol "char"

createVarDef :: Parser Type -> Parser String -> Parser Definition
createVarDef  parType parStr = Parser $ \s p -> case runParser parType s p of
  Right(typ, str, pos) -> case runParser parStr str pos of
    Right(name, string, position) -> Right ((VarDefinition name typ), string, position)
    Left a -> Left a
  Left a -> Left a


parseDeclareVar :: Parser Definition
parseDeclareVar = createVarDef (goodType <$> parseType) (parseDefinitionName)

parseParameter :: Parser (String, Type)
parseParameter =
  (swap <$> ((,) <$> typ <*> str))
    where
      typ = goodType <$> parseType
      str = parseDefinitionName

parseParameterWithComa :: Parser (String, Type)
parseParameterWithComa = parseParameter <* parseChar ','


parseParameters :: Parser [(String, Type)]
parseParameters =
  parseWithSpace
  (parseOpeningParenthesis
  *>  parseMany (parseWithSpace (parseParameterWithComa <|> parseParameter))
  <* parseClosingParenthesis)

