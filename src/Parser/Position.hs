{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module Parser.Position (Position (..), defaultPosition, moveCursor, Range (..), newRange, defaultRange, growRange) where

data Position = Position {line :: Int, char :: Int} deriving (Eq)

defaultPosition :: Position
defaultPosition = Position {line = 0, char = 0}

moveCursor :: Position -> Bool -> Position
moveCursor pos True = Position {line = line pos + 1, char = 0}
moveCursor pos False = Position {line = line pos, char = char pos + 1}

data Range = Range {start :: Position, end :: Position} deriving (Show, Eq)

instance Show Position where
  show pos = show (line pos) ++ ":" ++ show (char pos)

newRange :: Position -> Position -> Range
newRange x y = Range { start = x, end = y}

defaultRange :: Range
defaultRange = Range { start = defaultPosition , end = defaultPosition }

growRange :: Range -> Bool -> Range
growRange old nl = Range { start = start old , end = moveCursor (end old) nl }
