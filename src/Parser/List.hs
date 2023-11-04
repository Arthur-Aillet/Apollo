module Parser.List (module Parser.List) where

import Ast.Type (Operable (..))
import Control.Applicative (Alternative ((<|>)))
import Parser.Char (parseChar, parseClosingBraquet, parseOpeningBraquet, parseAChar, parseOpeningQuote, parseClosingQuote)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))
import {-# SOURCE #-} Parser.Operable (parseElement)
import Eval.Exec (Value (VAtom, VList), Atom (AtomB, AtomC))
import Parser.Int (parseFloat, parseInt)
import Parser.Bool (parseBool)
import Parser.Position (defaultPosition)

parseElemWithComa :: Parser Operable
parseElemWithComa = parseElement <* parseWithSpace (parseChar ',')

parseElems :: Parser [Operable]
parseElems = parseMany (parseWithSpace (parseElemWithComa <|> parseElement))

parseList :: Parser [Operable]
parseList = Parser $ \s p -> case runParser (parseWithSpace ( parseOpeningBraquet *> parseElems <* parseClosingBraquet)) s p of
  Right (elements, str, pos) -> Right (elements, str, pos)
  Left a -> Left a

----------------------------------------------------

getBoolAtom :: Parser Bool -> Parser Value
getBoolAtom parser = Parser $ \s p -> case runParser parser s p of
  Right (bool, str, pos) -> Right (VAtom $ AtomB bool, str, pos)
  Left a -> Left a

getcharAtom :: Parser Char -> Parser Value
getcharAtom parser = Parser $ \s p -> case runParser (parseOpeningQuote *> parser <* parseClosingQuote) s p of
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
parseAtomValue =  getFloatAtom parseFloat
              <|> getIntAtom parseInt
              <|> getBoolAtom parseBool
              <|> getcharAtom parseAChar

parseValsWithComa :: Parser Value
parseValsWithComa = parseAtomValue <* parseWithSpace (parseChar ',')

parsevals :: Parser [Value]
parsevals = parseMany (parseWithSpace (parseValsWithComa <|> parseAtomValue))

parseListValue :: Parser Value
parseListValue = Parser $ \s p -> case runParser (parseWithSpace ( parseOpeningBraquet *> parsevals <* parseClosingBraquet)) s p of
  Right (elements, str, pos) -> Right (VList elements, str, pos)
  Left a -> Left a

stringToVal :: String -> Maybe Value
stringToVal str = case runParser (parseWithSpace (parseAtomValue <|> parseListValue)) str defaultPosition of
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
