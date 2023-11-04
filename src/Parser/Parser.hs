{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Parser.hs
-}

module Parser.Parser (parser) where

import Ast.Type (Definition (..))
import Parser.Definition (parseFuncDefinition, parseManyFuncDefinition)
import Parser.Position (defaultPosition)
import Parser.Syntax (parseManyValidOrEmpty, parseManyStructure)
import Parser.Type (Parser (..))
import System.Exit (ExitCode (ExitFailure), exitWith)


parser :: String -> IO [Definition]
parser str = case runParser (parseManyFuncDefinition (parseManyStructure parseFuncDefinition)) str defaultPosition of
  Right (def, _, _) -> return def
  Left a -> print a >> exitWith (ExitFailure 1)
