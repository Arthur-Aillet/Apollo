--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Condition
--

module Parser.Condition (module Parser.Condition) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Type (Parser(..))
import Atom.Atom(Atom(..))
import Ast.Type(Ast (..), Function (..), Structure (..), Operation (..), Type (..), Definition (..), Operable (..))
import Parser.Symbol (parseSymbol)
import Parser.Int(parseInt)
import Parser.Char(parseAChar)
import Parser.Int(parseFloat)
import Parser.Bool(parseBool)
import Eval.Builtin (Builtin(..))
import Parser.StackTrace (StackTrace(..), defaultLocation)
import Parser.Range (Range(..))
import Parser.Syntax(parseMany, parseWithSpace)
import Parser.Char(parseAnyChar)

getPredicat :: String -> (Maybe Builtin)
getPredicat "<" = Just Less
getPredicat ">" = Nothing
getPredicat "==" = Just Eq
getPredicat "!=" = Nothing
getPredicat "<=" = Nothing
getPredicat ">=" = Nothing
getpredicat _ = Nothing

parsePredicat :: Parser String
parsePredicat = parseSymbol "<"
                <|> parseSymbol ">"
                <|> parseSymbol "=="
                <|> parseSymbol "!="
                <|> parseSymbol "<="
                <|> parseSymbol ">="

checkPredicat :: Parser String -> Parser Builtin
checkPredicat parser = Parser $ \s p -> case runParser parser s p of
  Right (predicatstr, str, pos) -> case getPredicat predicatstr of
    Just a -> Right (a, str, pos)
    Nothing -> Left (StackTrace [("Invalid Operand : ", Range p pos, defaultLocation)])
  Left a -> Left a

parseApredicat :: Parser Builtin
parseApredicat = Parser $ \s p -> case runParser (checkPredicat parsePredicat) s p of
  Right (result, str, pos) -> Right(result, str, pos)
  Left a -> Left a

parseCondOperation :: Parser Operation
parseCondOperation = Parser $ \s p -> case runParser parseOperable s p of
  Right (resultleft, newstrmiddle, newposmiddle) -> case runParser (parseWithSpace parseApredicat) newstrmiddle newposmiddle of
    Right (resultmiddle, newstrright, newposright) -> case runParser (parseWithSpace parseOperable) newstrright newposright of
      Right (resultright, newstr, newpos) -> Right (CallStd resultmiddle [OpValue resultright, OpValue resultleft], newstr, newpos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

getBoolAtom :: Parser Bool -> Parser Atom
getBoolAtom parser = Parser $ \s p -> case runParser parser s p of
  Right (bool, str, pos) -> Right (AtomB bool, str, pos)
  Left a -> Left a

getcharAtom :: Parser Char -> Parser Atom
getcharAtom parser = Parser $ \s p -> case runParser parser s p of
  Right (char, str, pos) -> Right (AtomC char False, str, pos)
  Left a -> Left a

parseOperable :: Parser Atom
parseOperable = parseFloat <|> parseInt <|> getBoolAtom parseBool <|> getcharAtom parseAChar
