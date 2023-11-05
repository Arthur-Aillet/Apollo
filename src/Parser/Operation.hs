{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Condition
-}

module Parser.Operation (module Parser.Operation) where

import Ast.Ast (Operable (..), Operation (..))
import Control.Applicative (Alternative ((<|>), empty))
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
parsePredicat =
  foldl
    (\prec symbol -> prec <|> parseSymbol symbol)
    empty
    ["+", "-", "*", "/", "%", "==", "<", "<=", ">", ">=", "!=", "&&", "||", ":"]

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
parseStd =
  (\left middle right -> CallStd middle [left, right])
    <$> parseMaybeparenthesis parseOperable
    <*> parseWithSpace (checkOperator parsePredicat getPredicat)
    <*> parseMaybeparenthesis parseOperable

parseUnaryOp :: Parser Operation
parseUnaryOp =
  CallStd
    <$> parseWithSpace (checkOperator parseUnary getUnary)
    <*> ((: []) <$> parseMaybeparenthesis parseOperable)

parseIndexOp :: Parser Operation
parseIndexOp =
  (\left middle right -> CallStd middle [left, right])
    <$> parseMaybeparenthesis parseOperable
    <*> parseWithSpace (checkOperator parseIndex getIndex)
    <*> (parseMaybeparenthesis parseOperable <* parseClosingBraquet)

parseBuiltinFct :: Parser Operation
parseBuiltinFct =
  CallStd
    <$> parseWithSpace (checkOperator parseBuiltin getBuiltin)
    <*> ((: []) <$> parseMaybeparenthesis parseOperable)

parseSysCallFct :: Parser Operation
parseSysCallFct =
  CallSys
    <$> parseWithSpace (checkOperator parseSysCall getSysCall)
    <*> ((: []) <$> parseMaybeparenthesis parseOperable)

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
parseFct =
  CallFunc
    <$> parseWithSpace (parseSymbol "@" *> parseDefinitionName)
    <*> (parseargs <|> parseNoargs)

parseSh :: Parser Operation
parseSh =
  CallSH
    <$> parseWithSpace (parseSymbol "$" *> parseDefinitionName)
    <*> (parseargs <|> parseNoargs)

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