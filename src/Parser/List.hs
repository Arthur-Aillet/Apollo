module Parser.List (module Parser.List) where

import Ast.Type (Operable (..))
import Control.Applicative (Alternative ((<|>)))
import Parser.Char (parseChar, parseClosingBraquet, parseOpeningBraquet)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))
import {-# SOURCE #-} Parser.Operable (parseElement)
-- import Debug.Trace (trace)

-- parseParameter :: Parser (String, Type)
-- parseParameter =
--   (swap <$> ((,) <$> typ <*> str))
--   where
--     typ = parseType
--     str = parseDefinitionName

-- parseParameterWithComa :: Parser (String, Type)
-- parseParameterWithComa = parseParameter <* parseChar ','

-- parseParameters :: Parser [(String, Type)]
-- parseParameters =
--   parseWithSpace
--     ( parseOpeningParenthesis
--         *> parseMany (parseWithSpace (parseParameterWithComa <|> parseParameter))
--         <* parseClosingParenthesis
--     )

parseElemWithComa :: Parser Operable
parseElemWithComa = parseElement <* parseWithSpace (parseChar ',')

parseElems :: Parser [Operable]
parseElems = parseMany (parseWithSpace (parseElemWithComa <|> parseElement))

parseList :: Parser [Operable]
parseList = Parser $ \s p -> case runParser (parseWithSpace ( parseOpeningBraquet *> parseElems <* parseClosingBraquet)) s p of
  Right (elements, str, pos) -> Right (elements, str, pos)
  Left a -> Left a
