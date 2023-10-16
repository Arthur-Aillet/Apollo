module Parser.Condition
  ( parseIf
  )
where

import Parser.Type (Parser(..))
import Ast.Type (Ast(..))
import Parser.Ast(parseAst)

parseIf :: Parser Ast
parseIf = If <$> parseAst <*> parseAst <*> parseAst
