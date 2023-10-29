{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST display
-}

module Ast.Display (compile) where

import Ast.Compile (Binary (..), generateBinary)
import Ast.Error (Compile (..), Error, Warning)
import Ast.Type (Definition)
import System.Exit (ExitCode (ExitFailure), exitWith)

yellow :: String
yellow = "\x1b[33m"

red :: String
red = "\x1b[31m"

resetColor :: String
resetColor = "\x1b[0m"

displayWarnings :: [Warning] -> IO ()
displayWarnings [] = pure ()
displayWarnings warns =
  putStrLn $ yellow ++ "Warnings:\n" ++ resetColor ++ concatMap ('\t' :) warns

displayError :: Error -> IO ()
displayError err =
  putStrLn $
    red ++ "Error during compilation:\n" ++ resetColor ++ "\t" ++ err

compile :: [Definition] -> IO Binary
compile defs = case generateBinary defs of
  Ko w err ->
    displayWarnings w
      >> displayError err
      >> exitWith (ExitFailure 1)
  Ok w bin ->
    displayWarnings w
      >> return bin
