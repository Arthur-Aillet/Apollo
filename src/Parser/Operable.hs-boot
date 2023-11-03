module Parser.Operable (module Parser.Operable) where

import Ast.Type (Operable (..), Type (..), Operation())
import Parser.Type (Parser (..))

parseDefinitionName :: Parser String

parseOpList :: Parser Operable

parseOperable :: Parser Operable

parseElement :: Parser Operable