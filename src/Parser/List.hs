{-
-- EPITECH PROJECT, 2023
-- Apollo
-- File description:
-- Parse list
-}

module Parser.List (module Parser.List) where

import Ast.Ast (Operable (..))
import Control.Applicative (Alternative ((<|>)))
import Eval.Exec (Atom (AtomB, AtomC), Value (VAtom, VList))
import Parser.Bool (parseBool)
import Parser.Char (parseAChar, parseChar, parseClosingBraquet, parseClosingQuote, parseOpeningBraquet, parseOpeningQuote)
import Parser.Int (parseFloat, parseInt)
import {-# SOURCE #-} Parser.Operable (parseElement)
import Parser.Position (defaultPosition)
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
