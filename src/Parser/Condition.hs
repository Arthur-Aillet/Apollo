--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Condition
--

module Parser.Condition (module Parser.Condition) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Type (Parser(..))
import Ast.Type(Ast (..), Function (..), Structure (..), Operation (..), Type (..), Definition (..), Operable (..))
import Parser.Symbol (parseSymbol)
import Parser.Int(parseInt)
import Parser.Char(parseAChar, parseOpeningParenthesis, parseClosingParenthesis)
import Parser.Int(parseFloat)
import Parser.Bool(parseBool)
import Eval.Atom (Atom (..))
import Eval.Operator (Operator(..))
import Parser.StackTrace (StackTrace(..), defaultLocation)
import Parser.Range (Range(..))
import Parser.Syntax(parseMany, parseWithSpace)
import Parser.Char(parseAnyChar)

getPredicat :: String -> (Maybe Operator)
getPredicat "<" = Just Lt
getPredicat ">" = Just Gt
getPredicat "==" = Just Eq
getPredicat "!=" = Just NEq
getPredicat "<=" = Just LEt
getPredicat ">=" = Just GEt
getpredicat _ = Nothing

getBoolOperator :: String -> (Maybe Operator)
getBoolOperator "==" = Just Eq
getBoolOperator "&&" = Just BAnd
getBoolOperator "||" = Just BOr
getBoolOperator _ = Nothing

parseBoolOperator :: Parser String
parseBoolOperator = parseSymbol "=="
                  <|> parseSymbol "&&"
                  <|> parseSymbol "||"

parsePredicat :: Parser String
parsePredicat = parseSymbol "<"
                <|> parseSymbol ">"
                <|> parseSymbol "=="
                <|> parseSymbol "!="
                <|> parseSymbol "<="
                <|> parseSymbol ">="

checkBoolOperator :: Parser String -> Parser Operator
checkBoolOperator parser = Parser $ \s p -> case runParser parser s p of
  Right (predicatstr, str, pos) -> case getBoolOperator predicatstr of
    Just a -> Right (a, str, pos)
    Nothing -> Left (StackTrace [("Invalid operator : ", Range p pos, defaultLocation)])
  Left a -> Left a

checkPredicat :: Parser String -> Parser Operator
checkPredicat parser = Parser $ \s p -> case runParser parser s p of
  Right (predicatstr, str, pos) -> case getPredicat predicatstr of
    Just a -> Right (a, str, pos)
    Nothing -> Left (StackTrace [("Invalid operator : ", Range p pos, defaultLocation)])
  Left a -> Left a

parseABoolOperator :: Parser Operator
parseABoolOperator = Parser $ \s p -> case runParser (checkBoolOperator parseBoolOperator) s p of
  Right (result, str, pos) -> Right(result, str, pos)
  Left a -> Left a

parseApredicat :: Parser Operator
parseApredicat = Parser $ \s p -> case runParser (checkPredicat parsePredicat) s p of
  Right (result, str, pos) -> Right(result, str, pos)
  Left a -> Left a

parseAtomCondOperation :: Parser Operation
parseAtomCondOperation = Parser $ \s p -> case runParser parseOperable s p of
  Right (resultleft, newstrmiddle, newposmiddle) -> case runParser (parseWithSpace parseApredicat) newstrmiddle newposmiddle of
    Right (resultmiddle, newstrright, newposright) -> case runParser (parseWithSpace parseOperable) newstrright newposright of
      Right (resultright, newstr, newpos) -> Right (CallStd resultmiddle [OpValue resultleft, OpValue resultright], newstr, newpos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseBooleanOperation :: Parser Operation
parseBooleanOperation = Parser $ \s p -> case runParser parseAtomCondOperation s p of
  Right (resultLeft, newstrmiddle, newposmiddle) -> case runParser (parseWithSpace parseABoolOperator) newstrmiddle newposmiddle of
    Right (resultmiddle, newstrright, newposright) -> case runParser (parseWithSpace parseAtomCondOperation) newstrright newposright of
      Right (resultright, newstr, newpos) -> Right (CallStd resultmiddle [OpOperation resultLeft, OpOperation resultright], newstr, newpos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseCondOperation :: Parser Operation
parseCondOperation =  parseAtomCondOperation
                  <|> parseBooleanOperation
                  <|> parseOpeningParenthesis *> parseWithSpace parseBooleanOperation <* parseClosingParenthesis
                  <|> parseOpeningParenthesis *> parseWithSpace parseCondOperation <* parseClosingParenthesis

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

parseBoolOperable :: Parser Atom
parseBoolOperable = getBoolAtom parseBool
