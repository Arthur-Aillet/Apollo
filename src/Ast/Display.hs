{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST display
-}

module Ast.Display (displayWarnings) where

import Ast.Error (Warning)

yellow :: String
yellow = "\x1b[33m"

resetColor :: String
resetColor = "\x1b[0m"

displayWarnings :: [Warning] -> IO ()
displayWarnings warns =
  putStrLn $ yellow ++ "Warnings:\n" ++ resetColor ++ concatMap ('\t' :) warns
