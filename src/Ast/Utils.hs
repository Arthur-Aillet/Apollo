{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts Utils
-}

module Ast.Utils ((+++), (++++), concatInner, listInner) where

import Ast.Error (Compile (..))

(+++) :: [a] -> [a] -> [a] -> [a]
(+++) a b c = a ++ b ++ c

(++++) :: [a] -> [a] -> [a] -> [a] -> [a]
(++++) a b c d = a ++ b ++ c ++ d

concatInner :: [Compile [b]] -> Compile [b]
concatInner = foldl (\a b -> (++) <$> a <*> b) (Ok [] [])

listInner :: [Compile b] -> Compile [b]
listInner = foldl (\a b -> (++) <$> a <*> ((: []) <$> b)) (Ok [] [])
