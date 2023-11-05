{-
-- EPITECH PROJECT, 2023
-- Apollo2
-- File description:
-- Parser.hs
-}

module Parser.Parser (parser) where

import Ast.Ast (Definition (..))
import Parser.Definition (parseFuncDefinition, parseManyFuncDef)
import Parser.Position (defaultPosition)
import Parser.Syntax (parseManyStructure)
import Parser.Type (Parser (..))
import System.Exit (ExitCode (ExitFailure), exitWith)

parser :: String -> IO [Definition]
parser str = case newparser of
  Right (def, _, _) -> return def
  Left a -> print a >> exitWith (ExitFailure 1)
  where
    newparser =
      runParser
        ( parseManyFuncDef (parseManyStructure parseFuncDefinition)
        )
        str
        defaultPosition