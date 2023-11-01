--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Condition
--

module Parser.Condition (module Parser.Condition) where

import Ast.Type (Ast (..), Definition (..), Function (..), Operable (..), Operation (..), Structure (..), Type (..))
import Control.Applicative (Alternative ((<|>)))
import Eval.Atom (Atom (..))
import Eval.Operator (Operator (..))
import Parser.Bool (parseBool)
import Parser.Char (parseAChar, parseAnyChar, parseClosingParenthesis, parseOpeningParenthesis)
import Parser.Int (parseFloat, parseInt)
import Parser.Range (Range (..))
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Symbol (parseSymbol)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))

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

getUnary :: String -> Maybe Operator
getUnary "!" = Just BNot

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
parseUnary = parseSymbol "!"

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

parseApredicat :: Parser Operator
parseApredicat = Parser $ \s p -> case runParser (checkPredicat parsePredicat) s p of
  Right (result, str, pos) -> Right (result, str, pos)
  Left a -> Left a

parseAUnary :: Parser Operator
parseAUnary = Parser $ \s p -> case runParser (checkUnary parseUnary) s p of
  Right (result, str, pos) -> Right (result, str, pos)
  Left a -> Left a


-- parseAtomOperation :: Parser Operation
-- parseAtomOperation = Parser $ \s p -> case runParser parseOperable s p of
--   Right (resultleft, newstrmiddle, newposmiddle) -> case runParser (parseWithSpace parseApredicat) newstrmiddle newposmiddle of
--     Right (resultmiddle, newstrright, newposright) -> case runParser (parseWithSpace parseOperable) newstrright newposright of
--       Right (resultright, newstr, newpos) -> Right (CallStd resultmiddle [OpValue resultleft, OpValue resultright], newstr, newpos)
--       Left a -> Left a
--     Left a -> Left a
--   Left a -> Left a

-- parseOpOperation :: Parser Operation
-- parseOpOperation = Parser $ \s p -> case runParser parseOperation s p of
--   Right (resultLeft, newstrmiddle, newposmiddle) -> case runParser (parseWithSpace parseApredicat) newstrmiddle newposmiddle of
--     Right (resultmiddle, newstrright, newposright) -> case runParser (parseWithSpace parseOperation) newstrright newposright of
--       Right (resultright, newstr, newpos) -> Right (CallStd resultmiddle [OpOperation resultLeft, OpOperation resultright], newstr, newpos)
--       Left a -> Left a
--     Left a -> Left a
--   Left a -> Left a

-- parseOperation :: Parser Operation
-- parseOperation =  parseAtomOperation
--                   <|> parseOpeningParenthesis *> parseWithSpace parseAtomOperation <* parseClosingParenthesis
--                   <|> parseOpOperation
--                   <|> parseOpeningParenthesis *> parseWithSpace parseOpOperation <* parseClosingParenthesis

-- parseOperation :: Parser Operation

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
    Right (resultright, newstr, newpo) -> Right (CallStd resultLeft [resultright], newstr, newpo)
    Left a -> Left a
  Left a -> Left a

parseOperation :: Parser Operation
parseOperation =  parseStd
              <|> parseUnaryOp
              -- <|> parseFct
              -- <|> parseSh

getBoolOpValue :: Parser Bool -> Parser Operable
getBoolOpValue parser = Parser $ \s p -> case runParser parser s p of
  Right (bool, str, pos) -> Right (OpValue $ AtomB bool, str, pos)
  Left a -> Left a

getcharOpValue :: Parser Char -> Parser Operable
getcharOpValue parser = Parser $ \s p -> case runParser parser s p of
  Right (char, str, pos) -> Right (OpValue $ AtomC char False, str, pos)
  Left a -> Left a

getIntOpValue :: Parser Atom -> Parser Operable
getIntOpValue parser = Parser $ \s p -> case runParser parser s p of
  Right (int, str, pos) -> Right (OpValue int, str, pos)
  Left a -> Left a

getFloatOpValue :: Parser Atom -> Parser Operable
getFloatOpValue parser = Parser $ \s p -> case runParser parser s p of
  Right (float, str, pos) -> Right (OpValue float, str, pos)
  Left a -> Left a

parseOpValue :: Parser Operable
parseOpValue =  getFloatOpValue parseFloat
            <|> getIntOpValue parseInt
            <|> getBoolOpValue parseBool
            <|> getcharOpValue parseAChar

getVarOpVar :: Parser String -> Parser Operable
getVarOpVar parser = Parser $ \s p -> case runParser parser s p of
  Right (var, str, pos) -> Right (OpVariable var, str, pos)
  Left a -> Left a

parseOpVar :: Parser Operable
parseOpVar = getVarOpVar (parseWithSpace (parseMany (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ "_-"))))

getOpOp :: Parser Operation -> Parser Operable
getOpOp parser = Parser $ \s p -> case runParser parser s p of
  Right (op, str, pos) -> Right (OpOperation op, str, pos)
  Left a -> Left a

parseOpOperation :: Parser Operable
parseOpOperation =  getOpOp parseOperation

parseOperable :: Parser Operable
parseOperable =   parseOpValue
              <|> parseOpVar
              <|> parseOpOperation
