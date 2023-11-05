{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module Parser.Position (Position (..), defaultPosition, moveCursor) where

import Ast.Display (green, resetColor)

data Position = Position {line :: Int, char :: Int} deriving (Eq)

defaultPosition :: Position
defaultPosition = Position {line = 0, char = 0}

moveCursor :: Position -> Bool -> Position
moveCursor pos True = Position {line = line pos + 1, char = 0}
moveCursor pos False = Position {line = line pos, char = char pos + 1}

instance Show Position where
  show pos = green ++ show (line pos) ++ ":" ++ show (char pos) ++ resetColor
