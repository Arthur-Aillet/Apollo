{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Definition.hs
-}

module Parser.Definition (parseDeclareVar) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Type (Parser(..))
import Ast.Type(Ast (..), Function (..), Structure (..), Operation (..), Type (..), Definition (..), Operable (..))
import Parser.Symbol (parseSymbol)
import Parser.StackTrace (StackTrace(..), defaultLocation)
import Parser.Range (Range(..))
import Parser.Syntax(parseMany, parseWithSpace)
import Parser.Char(parseAnyChar)


goodType :: String -> Type
goodType "int" = TypeInt
goodType "float" = TypeFloat
goodType "char" = TypeChar
goodType "bool" = TypeBool

parseType :: Parser String
parseType = Parser $ \s p -> runParser (parseSymbol "int" <|> parseSymbol "float" <|> parseSymbol "bool" <|> parseSymbol "char") s p

parseDeclareVar :: Parser Definition
parseDeclareVar = Parser $ \s p -> case runParser parseType s p of
    Right (tp, str, new_pos) -> case runParser (parseWithSpace (parseMany (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_-")))) str new_pos  of
        Right (var, end_str, end_pos) -> Right (VarDefinition var (goodType tp), end_str, end_pos)
        Left (StackTrace [((_, (Range _ end), src))]) -> Left (StackTrace [("Invalid Syntax: " ++ show s, Range p end, src)])
    Left (StackTrace [(_, (Range _ e), src)]) -> Left (StackTrace [("Invalid Syntax: this type doesn't exist: " ++ show s, Range p e, src)])

