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
import Parser.Int(parseInt)
import Parser.Error(failingWith)

parseDefinitionName :: Parser String
parseDefinitionName = parseWithSpace (parseMany (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_-")))

goodType :: String -> (Maybe Type)
goodType "int" = Just TypeInt
goodType "float" = Just TypeFloat
goodType "char" = Just TypeChar
goodType "bool" = Just TypeBool
goodType _ = Nothing

isgoodType :: Parser (Maybe Type) -> Parser Type
isgoodType parser = Parser $ \s p -> case runParser parser s p of
  Right (Just typ, str, pos) -> Right (typ, str, pos)
  Right (Nothing, _, pos) -> Left (StackTrace [("This type doesn't exist: ", (Range p pos), defaultLocation)])
  Left a -> Left a

parseType :: Parser String
parseType = parseSymbol "int" <|> parseSymbol "float" <|> parseSymbol "bool" <|> parseSymbol "char"

createVarDef :: Parser Type -> Parser String -> Parser Definition
createVarDef  parType parStr = Parser $ \s p -> case runParser parType s p of
  Right(typ, str, pos) -> case runParser parStr str pos of
    Right(name, string, position) -> Right ((VarDefinition name typ), string, position)
    Left a -> Left a
  Left a -> Left a


parseDeclareVar :: Parser Definition
parseDeclareVar = createVarDef (isgoodType (goodType <$> parseType)) (parseDefinitionName)

parseParameter :: Parser (String, Type)
parseParameter =
  (swap <$> ((,) <$> typ <*> str))
    where
      typ = isgoodType (goodType <$> parseType)
      str = parseDefinitionName

parseParameterWithComa :: Parser (String, Type)
parseParameterWithComa = parseParameter <* parseChar ','


parseParameters :: Parser [(String, Type)]
parseParameters =
  parseWithSpace
  (parseOpeningParenthesis
  *>  parseMany (parseWithSpace (parseParameterWithComa <|> parseParameter))
  <* parseClosingParenthesis)

parseOperable :: Parser Operable
parseOperable = OpValue <$> parseInt

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
parseFuncDefinition = Parser $ \s p -> case runParser (goodType <$> parseType) s p of
  Right (typ, str, pos) -> case runParser parseDefinitionName str pos of
    Right (name, string, position) -> case runParser (parseFunction typ) string position of
      Right (func, new_str, new_pos) -> Right ((FuncDefinition name func), new_str, new_pos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a