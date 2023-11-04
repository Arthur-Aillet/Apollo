module Parser.Operable (module Parser.Operable) where

import Ast.Type (Operable (..))
import Parser.Type (Parser (..))

parseDefinitionName :: Parser String

parseOpList :: Parser Operable

parseOperable :: Parser Operable

parseElement :: Parser Operable