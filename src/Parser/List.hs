module Parser.List (module Parser.List) where

import Ast.Ast (Operable (..))
import Control.Applicative (Alternative ((<|>)))
import Eval.Exec (Atom (AtomB, AtomC), Value (VAtom, VList))
import Parser.Bool (parseBool)
import Parser.Char (parseAChar, parseChar, parseClosingBraquet, parseClosingsQuote, parseOpeningBraquet, parseOpeningsQuote, parseAnyChar)
import Parser.Int (parseFloat, parseInt)
import {-# SOURCE #-} Parser.Operable (parseElement)
import Parser.Position (defaultPosition, moveCursor)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))
import Parser.String (acceptableCharacters)

parseElemWithComa :: Parser Operable
parseElemWithComa = parseElement <* parseWithSpace (parseChar ',')

parseElems :: Parser [Operable]
parseElems = parseMany (parseWithSpace (parseElemWithComa <|> parseElement))

parseList :: Parser [Operable]
parseList = Parser $ \s p -> case runParser (parseWithSpace (parseOpeningBraquet *> parseElems <* parseClosingBraquet)) s p of
  Right (elements, str, pos) -> Right (elements, str, pos)
  Left a -> Left a

----------------------------------------------------

getBoolAtom :: Parser Bool -> Parser Value
getBoolAtom parser = Parser $ \s p -> case runParser parser s p of
  Right (bool, str, pos) -> Right (VAtom $ AtomB bool, str, pos)
  Left a -> Left a

getcharAtom :: Parser Char -> Parser Value
getcharAtom parser = Parser $ \s p -> case runParser (parseOpeningsQuote *> parser <* parseClosingsQuote) s p of
  Right (char, str, pos) -> Right (VAtom $ AtomC char False, str, pos)
  Left a -> Left a

getIntAtom :: Parser Atom -> Parser Value
getIntAtom parser = Parser $ \s p -> case runParser parser s p of
  Right (int, str, pos) -> Right (VAtom int, str, pos)
  Left a -> Left a

getFloatAtom :: Parser Atom -> Parser Value
getFloatAtom parser = Parser $ \s p -> case runParser parser s p of
  Right (float, str, pos) -> Right (VAtom float, str, pos)
  Left a -> Left a

parseAtomValue :: Parser Value
parseAtomValue =
  getFloatAtom parseFloat
    <|> getIntAtom parseInt
    <|> getBoolAtom parseBool
    <|> getcharAtom parseAChar

parseValsWithComa :: Parser Value
parseValsWithComa = parseAtomValue <* parseWithSpace (parseChar ',')

parsevals :: Parser [Value]
parsevals = parseMany (parseWithSpace (parseValsWithComa <|> parseAtomValue))

parseListValue :: Parser Value
parseListValue = Parser $ \s p -> case runParser (parseWithSpace (parseOpeningBraquet *> parsevals <* parseClosingBraquet)) s p of
  Right (elements, str, pos) -> Right (VList elements, str, pos)
  Left a -> Left a

getSpecialChar :: Char -> Value
getSpecialChar 'n' = VAtom $ AtomC '\n' False
getSpecialChar 't' = VAtom $ AtomC '\t' False
getSpecialChar char = VAtom $ AtomC char False

parseStrValue :: Parser [Value]
parseStrValue = Parser $ \s p -> case runParser (parseAnyChar acceptableCharacters) s p of
  Right ('\\', nextchar : newstr, newpos) -> case runParser parseStrValue newstr (moveCursor (moveCursor newpos False) False) of
    Right (found, endstr, endpos) -> Right (getSpecialChar nextchar : found, endstr, endpos)
    Left a -> Left a
  Right (char, [], newpos) -> Right ([VAtom (AtomC char False)], [], newpos)
  Right (char, newstr, newpos) -> case runParser parseStrValue newstr newpos of
    Right (found, endstr, endpos) -> Right (VAtom (AtomC char False) : found, endstr, endpos)
    Left a -> Left a
  Left a -> Left a

parseStringValue :: Parser Value
parseStringValue = Parser $ \s p -> case runParser (parseWithSpace parseStrValue) s p of
  Right (elements, str, pos) -> Right (VList elements, str, pos)
  Left a -> Left a

stringToVal :: String -> Maybe Value
stringToVal str = case runParser (parseWithSpace (parseAtomValue <|> parseListValue <|> parseStringValue)) str defaultPosition of
  Right (result, _, _) -> Just result
  Left _ -> Nothing

hasNothing :: [Maybe a] -> Bool
hasNothing [] = False
hasNothing (Nothing : _) = True
hasNothing (_ : xs) = hasNothing xs

argsToMaybeValues :: [String] -> [Maybe Value]
argsToMaybeValues = map stringToVal

removeMaybe :: Maybe a -> a
removeMaybe (Just x) = x

removeMaybes :: [Maybe a] -> [a]
removeMaybes = map removeMaybe
