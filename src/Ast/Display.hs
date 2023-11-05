{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- AST display
-}

module Ast.Display (compile, resetColor, yellow, red, green) where

import Ast.Ast (Definition)
import Ast.CompileAST (Binary (..), generateBinary)
import Ast.Error (Compile (..), Error, Warning)
import System.Exit (ExitCode (ExitFailure), exitWith)

yellow :: String
yellow = "\x1b[33m"

red :: String
red = "\x1b[31m"

green :: String
green = "\x1b[32m"

resetColor :: String
resetColor = "\x1b[0m"

displayWarnings :: [Warning] -> IO ()
displayWarnings [] = pure ()
displayWarnings [warn] =
  putStrLn $
    yellow
      ++ "Warning found during compilation:\n"
      ++ resetColor
      ++ "\t"
      ++ warn
displayWarnings warns =
  putStrLn $
    yellow
      ++ "Warnings found during compilation:\n"
      ++ resetColor
      ++ concatMap (\x -> '\t' : x ++ "\n") warns

displayError :: [Error] -> IO ()
displayError [] = pure ()
displayError [err] =
  putStrLn $
    red ++ "Error found during compilation:\n" ++ resetColor ++ "\t" ++ err
displayError err =
  putStrLn $
    red
      ++ "Errors found during compilation:\n"
      ++ resetColor
      ++ concatMap (\x -> '\t' : x ++ "\n") err

compile :: [Definition] -> IO Binary
compile defs = case generateBinary defs of
  Ko w err ->
    displayWarnings w
      >> displayError err
      >> exitWith (ExitFailure 1)
  Ok w bin ->
    displayWarnings w
      >> return bin
