{-
-- EPITECH PROJECT, 2023
-- Apollo
-- File description:
-- Parse Symbol
-}

module Parser.Symbol (module Parser.Symbol) where

import Ast.Ast (Type (..))
import Control.Applicative (Alternative ((<|>)))
import Parser.Char (parseChar, parseClosingBraquet, parseOpeningBraquet)
import Parser.Range (Range (..))
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Type (Parser (..))

parseSymbol :: String -> Parser String
parseSymbol str
  | length str == 1 = Parser $ \s p ->
      case runParser (parseChar (head str)) s p of
        Right (char, new_str, new_pos) -> Right ([char], new_str, new_pos)
        Left err -> Left err
parseSymbol (x : xs) = Parser $ \s p -> case runParser (parseChar x) s p of
  Left err -> Left err
  Right (new, new_str, new_pos) ->
    case runParser (parseSymbol xs) new_str new_pos of
      Left err -> Left err
      Right (found, fd_str, fd_pos) -> Right (new : found, fd_str, fd_pos)
parseSymbol [] = Parser $ \s p -> Right ([], s, p)

goodType :: String -> Maybe Type
goodType "int" = Just TypeInt
goodType "float" = Just TypeFloat
goodType "char" = Just TypeChar
goodType "bool" = Just TypeBool
goodType "string" = Just (TypeList (Just TypeChar))
goodType ('[' : listtype) = Just (TypeList (goodType (init listtype)))
goodType _ = Nothing

isgoodType :: Parser (Maybe Type) -> Parser Type
isgoodType parser = Parser $ \s p -> case runParser parser s p of
  Right (Just typ, str, pos) -> Right (typ, str, pos)
  Right (Nothing, _, pos) ->
    Left (StackTrace [("This type doesn't exist: ", Range p pos, def)])
    where
      def = defaultLocation
  Left a -> Left a

parseListType :: Parser String
parseListType = Parser $ \s p -> case runParser parseOpeningBraquet s p of
  Right (opb, opbstr, opbpos) -> case runParser parseSymbolType opbstr opbpos of
    Right (typestr, typstr, typpos) -> case runParser parseClosingBraquet typstr typpos of
      Right (clb, clbstr, clbpos) -> Right (opb : typestr ++ [clb], clbstr, clbpos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseSymbolType :: Parser String
parseSymbolType =
  parseSymbol "int"
    <|> parseSymbol "float"
    <|> parseSymbol "bool"
    <|> parseSymbol "char"
    <|> parseSymbol "string"
    <|> parseListType

parseType :: Parser Type
parseType = isgoodType (goodType <$> parseSymbolType)

newTypeErr :: String -> Position -> Position -> StackTrace
newTypeErr typestr p pos =
  StackTrace
    [ ( "Invalid type : \""
          ++ typestr
          ++ "\"",
        Range p pos,
        defaultLocation
      )
    ]

parseMaybeType :: Parser (Maybe Type)
parseMaybeType = Parser $ \s p -> case runParser parseSymbolType s p of
  Right (typestr, str, pos) -> case goodType typestr of
    Just typ -> Right (Just typ, str, pos)
    Nothing -> Left (newTypeErr typestr p pos)
  Left _ -> Right (Nothing, s, p)
