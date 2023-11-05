{-
-- EPITECH PROJECT, 2023
-- Apollo
-- File description:
-- Parse list
-}

module Parser.List (module Parser.List) where

import Ast.Ast (Operable (..))
import Control.Applicative (Alternative ((<|>)))
import Parser.Char (parseChar, parseClosingBraquet, parseOpeningBraquet)
import {-# SOURCE #-} Parser.Operable (parseElement)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))

parseElemWithComa :: Parser Operable
parseElemWithComa = parseElement <* parseWithSpace (parseChar ',')

parseElems :: Parser [Operable]
parseElems = parseMany (parseWithSpace (parseElemWithComa <|> parseElement))

parseList :: Parser [Operable]
parseList = Parser $ \s p -> case runParser parser s p of
  Right (elements, str, pos) -> Right (elements, str, pos)
  Left a -> Left a
  where
    parser =
      parseWithSpace
        ( parseOpeningBraquet
            *> parseElems
            <* parseClosingBraquet
        )

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
parseListValue =
  VList
    <$> parseWithSpace
      ( parseOpeningBraquet
          *> parsevals
          <* parseClosingBraquet
      )

stringToVal :: String -> Maybe Value
stringToVal str =
  case runParser parse str defaultPosition of
    Right (result, _, _) -> Just result
    Left _ -> Nothing
  where
    parse = parseWithSpace (parseAtomValue <|> parseListValue)

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
