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
import Parser.Char (parseAChar, parseAnyChar, parseClosingParenthesis, parseOpeningParenthesis, parseOpeningQuote, parseClosingQuote, parseChar, parseClosingBraquet, parseOpeningBraquet)
import Parser.Int (parseFloat, parseInt)
import Parser.Range (Range (..))
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Symbol (parseSymbol)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))
import Debug.Trace (trace)

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

-- parse_expression_1(lhs, min_precedence)
--     lookahead := peek next token -
--     while lookahead is a binary operator whose precedence is >= min_precedence -
--         op := lookahead -
--         advance to next token
--         rhs := parse_primary ()
--         lookahead := peek next token
--         while lookahead is a binary operator whose precedence is greater
--                  than op's, or a right-associative operator
--                  whose precedence is equal to op's
--             rhs := parse_expression_1 (rhs, precedence of op + (1 if lookahead precedence is greater, else 0))
--             lookahead := peek next token
--         lhs := the result of applying op with operands lhs and rhs
--     return lhs

parseMaybeparenthesis :: Parser a -> Parser a
parseMaybeparenthesis parser =  parseWithSpace parser
                            <|> parseWithSpace (parseOpeningParenthesis *> parseWithSpace parser <* parseClosingParenthesis)

-- secondWhile :: Operable -> Int -> Operator -> Parser Operable
-- secondWhile rhs opprec nextop = Parser $ \s p -> if getPredicatPrecedence nextop > opprec
--   then
--     case runparser (precedenceClimbing rhs (opporec + 1)) s p of
--       Right (res, nextstr, nextpos) -> Right(case runparser (secondWhile ) nextstr nextpos of)
--       Left a -> a
--   else
--     Right (rhs, s, p)

-- firstWhile :: Operable -> Int -> Operator -> Parser Operable
-- firstWhile lhs prec operand = Parser $ \s p -> if getPredicatPrecedence operand >= prec
--   then
--     case runParser (parseMaybeparenthesis parseOperable) newstrright newposright of
--       Right (rhs, rstr, rpos) -> case runParser (parseWithSpace parseApredicat) newstrmiddle newposmiddle of
--         Right (nextop, opstr, oppos) -> case runparser (firstWhile (OpOperation op [lhs, case runParser (secondWhile rhs (getPredicatPrecedence operand) nextop) opstr oppos of
--           Right (res, secondwhilestr, secondewhilepos) -> res
--           Left a -> Left a
--           ]) prec (case runParser (parseWithSpace parseApredicat) secondwhilestr secondewhilepos of
--             Right (res, newopstr, newoppos) -> res
--             Left a -> Left a
--             ) newopstr newoppos of
--           Right (result, nextstr, nextpos) -> Right (result, nextstr, nextpos)
--           Left a -> Left a)
--         Left a -> Left a
--       Left a -> Left a
--   else
--     Right (lhs, s, p)

-- precedenceClimbing :: Operable -> Int -> Parser Operable
-- precedenceClimbing lhs prec = Parser $ \s p -> case runParser (parseWithSpace parseApredicat) newstrmiddle newposmiddle of
--   Right (operand, newstrright, newposright) -> case runparser (firstWhile lhs prec operand) s p of
--     Right (result, newstr, newpos) -> Right (result, newstr, newpos)
--     Left a -> Left a
--   Left a -> Left a

-- precedenceClimbing lhs prec = Parser $ \s p -> case runParser (parseWithSpace parseApredicat) newstrmiddle newposmiddle of
--   Right (operand, newstrright, newposright) -> if getPredicatPrecedence operand >= prec
--     then
--       case runParser (parseMaybeparenthesis parseOperable) newstrright newposright of
--         Right (rhs, newstr, newpos) -> case runParser (parseWithSpace parseApredicat) newstr newpos of
--           Right (nextoperand, newstrnext, newposnext) -> if getPredicatPrecedence nextoperand > getPredicatPrecedence operand
--             then
              
--             else
--           Left a -> Left a
--         Left a -> Left a
--     else
--       lhs
--   Left a -> Left a

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

getBoolOpValue :: Parser Bool -> Parser Operable
getBoolOpValue parser = Parser $ \s p -> case runParser parser s p of
  Right (bool, str, pos) -> Right (OpValue $ AtomB bool, str, pos)
  Left a -> Left a

getcharOpValue :: Parser Char -> Parser Operable
getcharOpValue parser = Parser $ \s p -> case runParser (parseOpeningQuote *> parser <* parseClosingQuote) s p of
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
  Right ([], str, pos) -> Left (StackTrace [("empty var name", Range p pos, defaultLocation)])
  Right (var, str, pos) -> Right (OpVariable var, str, pos)
  Left a -> Left a

parseOpVar :: Parser Operable
parseOpVar = getVarOpVar (parseWithSpace (parseMany (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ "_-"))))

getOpOp :: Parser Operation -> Parser Operable
getOpOp parser = Parser $ \s p -> case runParser parser s p of
  Right (op, str, pos) -> Right (OpOperation op, str, pos)
  Left a -> Left a

parseOpOperation :: Parser Operable
parseOpOperation = getOpOp parseOperation

-- getOpList :: Parser OpList
-- getOpList parser = Parser $ \s p -> case runParser parser s p of
--   Right (list, str, pos) -> Right (OpList list, str, pos)
--   Left a -> Left a

parseElement :: Parser Operable
parseElement = parseOpValue
            <|> parseOpVar
            <|> parseOpList
            <|> parseOpeningParenthesis *> parseOpOperation <* parseClosingParenthesis

parseElementWithComa :: Parser Operable
parseElementWithComa = parseWithSpace (parseChar ',') *> parseElement

parseElements :: Parser [Operable]
parseElements = parseWithSpace (parseMany (parseWithSpace parseElement <|> parseWithSpace parseElementWithComa))

parseList :: Parser [Operable]
parseList = Parser $ \s p -> case runParser (parseWithSpace parseOpeningBraquet *> parseElements <*parseClosingBraquet) s p of
  Right (elements, str, pos) -> Right (elements, str, pos)
  Left a -> Left a

getOpList :: Parser [Operable] -> Parser Operable
getOpList parser = Parser $ \s p -> case runParser parser s p of
  Right (list, str, pos) -> Right (OpList list, str, pos)
  Left a -> Left a

parseOpList :: Parser Operable
parseOpList = getOpList parseList

-- parseOperable :: Parser Operable
-- parseOperable = parseOpValue
--             <|> parseOpVar
--             -- <|> parseOpList
--             <|> parseOpOperation

parseOperable :: Parser Operable
parseOperable = parseOpValue
            <|> parseOpVar
            <|> parseOpList
            <|> parseOpeningParenthesis *> parseOpOperation <* parseClosingParenthesis
-- runParser parseOperation "1 + 1" defaultPosition
-- :break parseOpOperation 0
-- stack ghci
