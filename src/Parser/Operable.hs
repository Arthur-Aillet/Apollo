{-
-- EPITECH PROJECT, 2023
-- Apollo2
-- File description:
-- Operable.hs
-}

module Parser.Operable (module Parser.Operable) where

import Ast.Ast (Operable (..), Operation (), Type (..))
import Control.Applicative (Alternative ((<|>)))
import Eval.Atom (Atom (..))
import Parser.Bool (parseBool)
import Parser.Char (parseAChar, parseAnyChar, parseClosingParenthesis, parseClosingQuote, parseOpeningParenthesis, parseOpeningQuote)
import Parser.Int (parseFloat, parseInt)
import Parser.List (parseList)
import Parser.Operation (parseOperation)
-- import Debug.Trace

import Parser.Range (Range (Range))
import Parser.StackTrace (defaultLocation)
import Parser.Symbol (parseSymbol, parseType)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..), StackTrace (StackTrace))

defChars :: [Char]
defChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_-"

parseDefinitionName :: Parser String
parseDefinitionName = Parser $ \s p -> case runParser (parseMany (parseAnyChar defChars)) s p of
  Right ([], _, pos) -> Left (StackTrace [("empty definition", Range p pos, defaultLocation)])
  Right a -> Right a
  Left a -> Left a

parseCast :: Parser Type
parseCast = parseWithSpace (parseSymbol "as" *> parseWithSpace parseType)

parseOpCast :: Parser Operable
parseOpCast = Parser $ \s p -> case runParser
  ( parseOpValue
      <|> parseOpVar
      <|> parseOpList
      <|> parseOpeningParenthesis *> parseOpOperation <* parseClosingParenthesis
  )
  s
  p of
  Right (lhs, lstr, lpos) -> case runParser parseCast lstr lpos of
    Right (rhs, rstr, rpos) -> Right (OpCast lhs rhs, rstr, rpos)
    Left a -> Left a
  Left a -> Left a

---------------------------------------------

getBoolOpValue :: Parser Bool -> Parser Operable
getBoolOpValue parser = Parser $ \s p -> case runParser parser s p of
  Right (bool, str, pos) -> Right (OpValue $ AtomB bool, str, pos)
  Left a -> Left a

getcharOpValue :: Parser Char -> Parser Operable
getcharOpValue parser = Parser $ \s p ->
  case runParser (parseOpeningQuote *> parser <* parseClosingQuote) s p of
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
parseOpValue =
  getFloatOpValue parseFloat
    <|> getIntOpValue parseInt
    <|> getBoolOpValue parseBool
    <|> getcharOpValue parseAChar

---------------------------------------------

parseOpVar :: Parser Operable
parseOpVar = OpVariable <$> parseWithSpace parseDefinitionName

---------------------------------------------

getOpList :: Parser [Operable] -> Parser Operable
getOpList parser = Parser $ \s p -> case runParser parser s p of
  Right (list, str, pos) -> Right (OpList list, str, pos)
  Left a -> Left a

parseOpList :: Parser Operable
parseOpList = getOpList parseList

---------------------------------------------

getOpOp :: Parser Operation -> Parser Operable
getOpOp parser = Parser $ \s p -> case runParser parser s p of
  Right (op, str, pos) -> Right (OpOperation op, str, pos)
  Left a -> Left a

parseOpOperation :: Parser Operable
parseOpOperation = getOpOp parseOperation

---------------------------------------------

parseOperable :: Parser Operable
parseOperable =
  parseOpCast
    <|> parseOpValue
    <|> parseOpVar
    <|> parseOpList
    <|> parseOpeningParenthesis *> parseOpOperation <* parseClosingParenthesis

parseElement :: Parser Operable
parseElement = parseOpOperation <|> parseOperable
