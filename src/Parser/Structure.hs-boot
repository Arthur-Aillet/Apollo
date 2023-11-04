module Parser.Structure (module Parser.Structure) where
import Ast.Type (Ast)
import Parser.Type (Parser(..))

parseAstStructure :: Parser Ast

parseManyAst :: Parser [Ast]