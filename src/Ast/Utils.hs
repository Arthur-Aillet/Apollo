{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts Utils
-}

module Ast.Utils ((+++), (++++), concatInner, listInner) where

(+++) :: [a] -> [a] -> [a] -> [a]
(+++) a b c = a ++ b ++ c

(++++) :: [a] -> [a] -> [a] -> [a] -> [a]
(++++) a b c d = a ++ b ++ c ++ d

concatInner :: [Either a [b]] -> Either a [b]
concatInner = foldl (\a b -> (++) <$> a <*> b) (Right [])

listInner :: [Either a b] -> Either a [b]
listInner = foldl (\a b -> (++) <$> a <*> ((: []) <$> b)) (Right [])
