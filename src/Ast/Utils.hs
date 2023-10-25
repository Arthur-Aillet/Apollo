{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts Utils
-}

module Ast.Utils ((+++), (++++)) where

(+++) :: [a] -> [a] -> [a] -> [a]
(+++) a b c = a ++ b ++ c

(++++) :: [a] -> [a] -> [a] -> [a] -> [a]
(++++) a b c d = a ++ b ++ c ++ d
