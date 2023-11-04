module Parser.Structure (module Parser.Structure) where
import Ast.Ast (Ast)
import Parser.Type (Parser(..))

parseAstStructure :: Parser Ast

parseManyAst :: Parser [Ast]
