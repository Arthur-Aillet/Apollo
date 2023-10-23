module Parser.Condition
  ( parseIf,
  )
where

import Ast.Type (Ast (..))
import Parser.Ast (parseAst)
import Parser.Type (Parser (..))

parseIf :: Parser Ast
parseIf = If <$> parseAst <*> parseAst <*> parseAst
