module Parser.List (module Parser.List) where

import Ast.Type (Operable (..))
import Control.Applicative (Alternative ((<|>)))
import Parser.Char (parseChar, parseClosingBraquet, parseOpeningBraquet)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))
import {-# SOURCE #-} Parser.Operable (parseOperable)

parseOpWithComa :: Parser Operable
parseOpWithComa = parseWithSpace (parseChar ',') *> parseOperable

parseOps :: Parser [Operable]
parseOps = parseWithSpace (parseMany (parseWithSpace parseOperable <|> parseWithSpace parseOpWithComa))

parseList :: Parser [Operable]
parseList = Parser $ \s p -> case runParser (parseWithSpace parseOpeningBraquet *> parseOps <*parseClosingBraquet) s p of
  Right (elements, str, pos) -> Right (elements, str, pos)
  Left a -> Left a