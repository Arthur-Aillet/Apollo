{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- AST To Insts Utils
-}

module Ast.Utils ((+++), (++++), concatInner, listInner, listInnerMaybe, allEqual, zip5, zip4) where

import Ast.Error (Compile (..))

(+++) :: [a] -> [a] -> [a] -> [a]
(+++) a b c = a ++ b ++ c

(++++) :: [a] -> [a] -> [a] -> [a] -> [a]
(++++) a b c d = a ++ b ++ c ++ d

concatInner :: [Compile [b]] -> Compile [b]
concatInner = foldl (\a b -> (++) <$> a <*> b) (Ok [] [])

listInner :: [Compile b] -> Compile [b]
listInner = foldl (\a b -> (++) <$> a <*> ((: []) <$> b)) (Ok [] [])

listInnerMaybe :: [Either a b] -> Either a [b]
listInnerMaybe = foldl (\a b -> (++) <$> a <*> ((: []) <$> b)) (Right [])

zip5 :: a -> b -> c -> d -> e -> (a, b, c, d, e)
zip5 a b c d e = (a, b, c, d, e)

zip4 :: a -> b -> c -> d -> (a, b, c, d)
zip4 a b c d = (a, b, c, d)

allEqual :: (Eq a) => [a] -> Bool
allEqual array = and $ zipWith (==) array (tail array)
