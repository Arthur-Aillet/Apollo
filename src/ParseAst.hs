module ParseAst
  ( parseAst
  )
where

import Parser.Type (Parser(..))
import Ast.Type (Ast(..))
import Parser.Bool (parseBool)

parseAst :: Parser Ast
parseAst = Truth <$> parseBool
