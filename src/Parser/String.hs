{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- String
-}

module Parser.String (module Parser.String) where

import Ast.Ast (Operable (OpValue))
import Eval.Atom (Atom (AtomC))
import Parser.Char (parseAnyChar, parseClosingQuote, parseOpeningQuote)
import Parser.Position (Position (..), moveCursor)
import Parser.Syntax (parseWithSpace)
import Parser.Type (Parser (..))

exInput :: String
exInput = "\"test\""

acceptableCharacters :: [Char]
acceptableCharacters =
  ['a' .. 'z']
    ++ ['A' .. 'Z']
    ++ ['0' .. '9']
    ++ "|/\\[](){}-_\"\' <>,./>?!@#$%^&*()+=[]"

getSpecialChar :: Char -> Operable
getSpecialChar 'n' = OpValue $ AtomC '\n' False
getSpecialChar 't' = OpValue $ AtomC '\t' False
getSpecialChar c = OpValue $ AtomC c False

moveAfterBackslash :: Position -> Position
moveAfterBackslash newpos = (moveCursor (moveCursor newpos False) False)

checkAfterBackslash :: Char -> Parser [Operable]
checkAfterBackslash nextchar = Parser $ \s p ->
  case runParser parseStr s (moveAfterBackslash p) of
    Right (found, endstr, endpos) ->
      Right (getSpecialChar nextchar : found, endstr, endpos)
    Left a -> Left a

parseStr :: Parser [Operable]
parseStr = Parser $ \s p ->
  case runParser (parseAnyChar acceptableCharacters) s p of
    Right ('\"', _, _) -> Right ([], s, p)
    Right ('\\', nextchar : newstr, newpos) ->
      runParser (checkAfterBackslash nextchar) newstr newpos
    Right (c, newstr, newpos) -> case runParser parseStr newstr newpos of
      Right (found, endstr, endpos) ->
        Right (OpValue (AtomC c False) : found, endstr, endpos)
      Left a -> Left a
    Left a -> Left a

parseBetweenQuotes :: Parser a -> Parser a
parseBetweenQuotes parser = parseOpeningQuote *> parser <* parseClosingQuote

parseString :: Parser [Operable]
parseString = parseWithSpace (parseBetweenQuotes parseStr)
