module Parser.Ast
  ( parseAst,
  )
where

import Ast.Type (Ast (..))
import Parser.Bool (parseBool)
import Parser.Type (Parser (..))

parseAst :: Parser Ast
parseAst = Truth <$> parseBool
