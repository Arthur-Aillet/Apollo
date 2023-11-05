{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Condition
-}

module Parser.Operation (module Parser.Operation) where

import Ast.Ast (Operable (..), Operation (..))
import Control.Applicative (Alternative ((<|>)))
import Eval.Operator (Operator (..))
import Eval.Syscall (Syscall (Print))
import Parser.Char (parseChar, parseClosingBraquet, parseClosingParenthesis, parseOpeningParenthesis)
import {-# SOURCE #-} Parser.Operable (parseDefinitionName, parseOperable)
import Parser.Range (Range (..))
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Symbol (parseSymbol)
import Parser.Syntax (parseMany, parseMaybeparenthesis, parseWithSpace)
import Parser.Type (Parser (..), faillingParser)

getPredicat :: String -> Maybe Operator
getPredicat "+" = Just Add
getPredicat "-" = Just Sub
getPredicat "*" = Just Mul
getPredicat "/" = Just Div
getPredicat "%" = Just Mod
getPredicat "==" = Just Eq
getPredicat "<" = Just Lt
getPredicat "<=" = Just LEt
getPredicat ">" = Just Gt
getPredicat ">=" = Just GEt
getPredicat "!=" = Just NEq
getPredicat "&&" = Just And
getPredicat "||" = Just Or
getPredicat ":" = Just Concat
getPredicat _ = Nothing

parsePredicat :: Parser String
parsePredicat = foldl
  (\prec symbol -> prec <|> parseSymbol symbol)
  faillingParser
  ["+","-","*","/","%","==","<","<=",">",">=","!=","&&","||",":"]

---------------------------------------------

getUnary :: String -> Maybe Operator
getUnary "!" = Just Not
getUnary _ = Nothing

parseUnary :: Parser String
parseUnary = parseSymbol "!"

---------------------------------------------

getIndex :: String -> Maybe Operator
getIndex "[" = Just Get
getIndex _ = Nothing

parseIndex :: Parser String
parseIndex = parseSymbol "["

---------------------------------------------

getBuiltin :: String -> Maybe Operator
getBuiltin "len" = Just Len
getBuiltin _ = Nothing

parseBuiltin :: Parser String
parseBuiltin = parseSymbol "len"

---------------------------------------------

getSysCall :: String -> Maybe Syscall
getSysCall "print" = Just Print
getSysCall _ = Nothing

parseSysCall :: Parser String
parseSysCall = parseSymbol "print"

---------------------------------------------

checkOperator :: Parser String -> (String -> Maybe a) -> Parser a
checkOperator parser getter = Parser $ \s p -> case runParser parser s p of
  Right (operatorstr, str, pos) -> case getter operatorstr of
    Just a -> Right (a, str, pos)
    Nothing ->
      Left
        ( StackTrace
            [ ( "Invalid operator : \""
                  ++ operatorstr
                  ++ "\"",
                Range p pos,
                defaultLocation
              )
            ]
        )
  Left a -> Left a

---------------------------------------------

parseStd :: Parser Operation
parseStd = Parser $ \s p -> case runParser (parseMaybeparenthesis parseOperable) s p of
  Right (resultLeft, newstrmiddle, newposmiddle) -> case runParser (parseWithSpace $ checkOperator parsePredicat getPredicat) newstrmiddle newposmiddle of
    Right (resultmiddle, newstrright, newposright) -> case runParser (parseMaybeparenthesis parseOperable) newstrright newposright of
      Right (resultright, newstr, newpos) -> Right (CallStd resultmiddle [resultLeft, resultright], newstr, newpos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseUnaryOp :: Parser Operation
parseUnaryOp = Parser $ \s p ->
  case runParser (parseWithSpace $ checkOperator parseUnary getUnary) s p of
    Right (left, str2, pos2) ->
      case runParser (parseMaybeparenthesis parseOperable) str2 pos2 of
        Right (right, str3, pos3) ->
          Right (CallStd left [right], str3, pos3)
        Left a -> Left a
    Left a -> Left a

parseIndexOp :: Parser Operation
parseIndexOp = Parser $ \s p ->
  case runParser (parseMaybeparenthesis parseOperable) s p of
    Right (left, str2, pos2) ->
      case runParser (parseWithSpace $ checkOperator parseIndex getIndex) str2 pos2 of
        Right (middle, str3, pos3) -> case runParser (parseMaybeparenthesis parseOperable <* parseClosingBraquet) str3 pos3 of
          Right (right, str4, pos4) -> Right (CallStd middle [left, right], str4, pos4)
          Left a -> Left a
        Left a -> Left a
    Left a -> Left a

parseBuiltinFct :: Parser Operation
parseBuiltinFct = Parser $ \s p ->
  case runParser (parseWithSpace $ checkOperator parseBuiltin getBuiltin) s p of
    Right (fct, fctstr, fctpos) ->
      case runParser (parseMaybeparenthesis parseOperable) fctstr fctpos of
        Right (op, opstr, oppos) -> Right (CallStd fct [op], opstr, oppos)
        Left a -> Left a
    Left a -> Left a

parseSysCallFct :: Parser Operation
parseSysCallFct = Parser $ \s p ->
  case runParser (parseWithSpace $ checkOperator parseSysCall getSysCall) s p of
    Right (fct, fctstr, fctpos) ->
      case runParser (parseMaybeparenthesis parseOperable) fctstr fctpos of
        Right (op, opstr, oppos) -> Right (CallSys fct [op], opstr, oppos)
        Left a -> Left a
    Left a -> Left a

---------------------------------------------

parseargWithComa :: Parser Operable
parseargWithComa = parseWithSpace (parseChar ',') *> parseOperable

parseargs :: Parser [Operable]
parseargs =
  parseOpeningParenthesis
    *> parseMany
      ( parseWithSpace parseOperable
          <|> parseWithSpace parseargWithComa
      )
    <* parseClosingParenthesis

parseNoargs :: Parser [Operable]
parseNoargs = Parser $ \s p -> Right ([], s, p)

parseFct :: Parser Operation
parseFct = Parser $ \s p -> case runParser (parseWithSpace (parseSymbol "@" *> parseDefinitionName)) s p of
  Right (name, nstr, npos) -> case runParser (parseargs <|> parseNoargs) nstr npos of
    Right (args, astr, apos) -> Right (CallFunc name args, astr, apos)
    Left a -> Left a
  Left a -> Left a

parseSh :: Parser Operation
parseSh = Parser $ \s p -> case runParser (parseWithSpace (parseSymbol "$" *> parseDefinitionName)) s p of
  Right (name, nstr, npos) -> case runParser (parseargs <|> parseNoargs) nstr npos of
    Right (args, astr, apos) -> Right (CallSH name args, astr, apos)
    Left a -> Left a
  Left a -> Left a

parseCall :: Parser Operation
parseCall = parseFct <|> parseSh

---------------------------------------------

parseOperation :: Parser Operation
parseOperation =
  parseStd
    <|> parseUnaryOp
    <|> parseIndexOp
    <|> parseBuiltinFct
    <|> parseSysCallFct
    <|> parseCall
