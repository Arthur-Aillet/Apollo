{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- parseBool
-}

module Parser.Bool (parseBool) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Symbol (parseSymbol)
import Parser.Type (Parser (..))

parseBool :: Parser Bool
parseBool = (== "True") <$> (parseSymbol "True" <|> parseSymbol "False")
