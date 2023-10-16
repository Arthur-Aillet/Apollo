module Parser.Condition
  ( parseIf
  )
where

import Parser.Type (Parser(..))
import Ast.Type (Ast(..))
import Parser.Bool (parseBool)

parseIf :: Parser Ast
parseIf = If <$> parseAst <*> parseAst <*> parseAst
