{-
-- EPITECH PROJECT, 2023
-- Apollo
-- File description:
-- parseBool
-}

module Parser.Bool (parseBool) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Symbol (parseSymbol)
import Parser.Type (Parser (..))

parseBool :: Parser Bool
parseBool = (== "true") <$> (parseSymbol "true" <|> parseSymbol "false")
