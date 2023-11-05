{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module Parser.Range (Range (..), defaultRange) where

import Parser.Position (Position (..), defaultPosition)

data Range = Range Position Position deriving (Show, Eq)

defaultRange :: Range
defaultRange = Range defaultPosition defaultPosition
