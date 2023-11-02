--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Condition
--

module Parser.Operation (module Parser.Operation) where

import Ast.Type (Ast (..), Definition (..), Function (..), Operable (..), Operation (..), Structure (..), Type (..))
import Control.Applicative (Alternative ((<|>)))
import Eval.Atom (Atom (..))
import Eval.Operator (Operator (..))
import Parser.Bool (parseBool)
import Parser.Char (parseAChar, parseAnyChar, parseClosingParenthesis, parseOpeningParenthesis, parseOpeningQuote, parseClosingQuote, parseChar, parseClosingBraquet, parseOpeningBraquet)
import Parser.Int (parseFloat, parseInt)
import Parser.Range (Range (..))
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Symbol (parseSymbol)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))
import Debug.Trace (trace)
import {-# SOURCE #-} Parser.Operable (parseOperable)

getPredicat :: String -> Maybe Operator
getPredicat "+" = Just Add
getPredicat "-" = Just Sub
getPredicat "*" = Just Mul
getPredicat "/" = Just Div
getPredicat "%" = Just Mod
getPredicat "&&" = Just BAnd
getPredicat "||" = Just BOr
getPredicat "==" = Just Eq
getPredicat "<" = Just Lt
getPredicat "<=" = Just LEt
getPredicat ">" = Just Gt
getPredicat ">=" = Just GEt
getPredicat "!=" = Just NEq
getPredicat _ = Nothing

getPredicatPrecedence :: Operator -> Int
getPredicatPrecedence Add = 2
getPredicatPrecedence Sub = 2
getPredicatPrecedence Mul = 3
getPredicatPrecedence Div = 3
getPredicatPrecedence Mod = 3
getPredicatPrecedence BAnd = 0
getPredicatPrecedence BOr = 0
getPredicatPrecedence Eq = 1
getPredicatPrecedence Lt = 1
getPredicatPrecedence LEt = 1
getPredicatPrecedence Gt = 1
getPredicatPrecedence GEt = 1
getPredicatPrecedence NEq = 1
getPredicatPrecedence _ = -1

getUnary :: String -> Maybe Operator
getUnary "++" = Just Incr
getUnary "--" = Just Decr
getUnary "!" = Just BNot
getUnary _ = Nothing

getIndex :: String -> Maybe Operator
getIndex "[" = Just Add
getIndex _ = Nothing

parsePredicat :: Parser String
parsePredicat = parseSymbol "+"
            <|> parseSymbol "-"
            <|> parseSymbol "*"
            <|> parseSymbol "/"
            <|> parseSymbol "%"
            <|> parseSymbol "&&"
            <|> parseSymbol "||"
            <|> parseSymbol "=="
            <|> parseSymbol "<"
            <|> parseSymbol "<="
            <|> parseSymbol ">"
            <|> parseSymbol ">="
            <|> parseSymbol "!="

parseUnary :: Parser String
parseUnary =  parseSymbol "++"
          <|> parseSymbol "--"
          <|> parseSymbol "!"

parseIndex :: Parser String
parseIndex = parseSymbol "["

checkPredicat :: Parser String -> Parser Operator
checkPredicat parser = Parser $ \s p -> case runParser parser s p of
  Right (predicatstr, str, pos) -> case getPredicat predicatstr of
    Just a -> Right (a, str, pos)
    Nothing -> Left (StackTrace [("Invalid operator : ", Range p pos, defaultLocation)])
  Left a -> Left a

checkUnary :: Parser String -> Parser Operator
checkUnary parser = Parser $ \s p -> case runParser parser s p of
  Right (unarystr, str, pos) -> case getUnary unarystr of
    Just a -> Right (a, str, pos)
    Nothing -> Left (StackTrace [("Invalid operator : ", Range p pos, defaultLocation)])
  Left a -> Left a

checkIndex :: Parser String -> Parser Operator
checkIndex parser = Parser $ \s p -> case runParser parser s p of
  Right (indexstr, str, pos) -> case getIndex indexstr of
    Just a -> Right (a, str, pos)
    Nothing -> Left (StackTrace [("Invalid operator : ", Range p pos, defaultLocation)])
  Left a -> Left a

parseApredicat :: Parser Operator
parseApredicat = Parser $ \s p -> case runParser (checkPredicat parsePredicat) s p of
  Right (result, str, pos) -> Right (result, str, pos)
  Left a -> Left a

parseAUnary :: Parser Operator
parseAUnary = Parser $ \s p -> case runParser (checkUnary parseUnary) s p of
  Right (result, str, pos) -> Right (result, str, pos)
  Left a -> Left a

parseAIndex :: Parser Operator
parseAIndex = Parser $ \s p -> case runParser (checkIndex parseIndex) s p of
  Right (result, str, pos) -> Right (result, str, pos)
  Left a -> Left a

parseMaybeparenthesis :: Parser a -> Parser a
parseMaybeparenthesis parser =  parseWithSpace parser
                            <|> parseWithSpace (parseOpeningParenthesis *> parseWithSpace parser <* parseClosingParenthesis)

parseStd :: Parser Operation
parseStd = Parser $ \s p -> case runParser (parseMaybeparenthesis parseOperable) s p of
  Right (resultLeft, newstrmiddle, newposmiddle) -> case runParser (parseWithSpace parseApredicat) newstrmiddle newposmiddle of
    Right (resultmiddle, newstrright, newposright) -> case runParser (parseMaybeparenthesis parseOperable) newstrright newposright of
      Right (resultright, newstr, newpos) -> Right (CallStd resultmiddle [resultLeft, resultright], newstr, newpos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseUnaryOp :: Parser Operation
parseUnaryOp = Parser $ \s p -> case runParser (parseWithSpace parseAUnary) s p of
  Right (resultLeft, newstrright, newposright) -> case runParser (parseMaybeparenthesis parseOperable) newstrright newposright of
    Right (resultright, newstr, newpos) -> Right (CallStd resultLeft [resultright], newstr, newpos)
    Left a -> Left a
  Left a -> Left a

parseIndexOp :: Parser Operation
parseIndexOp =  Parser $ \s p -> case runParser (parseMaybeparenthesis parseOperable) s p of
  Right (resultLeft, newstrmiddle, newposmiddle) -> case runParser (parseWithSpace parseAIndex) newstrmiddle newposmiddle of
    Right (resultmiddle, newstrright, newposright) -> trace newstrright $ case runParser (parseMaybeparenthesis parseOperable <* parseClosingBraquet) newstrright newposright of
      Right (resultright, newstr, newpos) -> Right (CallStd resultmiddle [resultLeft, resultright], newstr, newpos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseOperation :: Parser Operation
parseOperation = parseStd
              <|> parseUnaryOp
              <|> parseIndexOp
              -- <|> parseFct
              -- <|> parseSh
