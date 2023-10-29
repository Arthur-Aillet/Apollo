{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST display
-}

module Ast.Display (displayWarnings, displayError) where

import Ast.Error (Error, Warning)

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
