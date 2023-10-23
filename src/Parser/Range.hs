{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module Parser.Range (Range (..), newRange, defaultRange, growRange, addNewMessage) where

import Parser.Position (Position (..), defaultPosition, moveCursor)

data Range = Range {start :: Position, end :: Position} deriving (Show, Eq)

newRange :: Position -> Position -> Range
newRange x y = Range {start = x, end = y}

defaultRange :: Range
defaultRange = Range {start = defaultPosition, end = defaultPosition}

growRange :: Range -> Bool -> Range
growRange old nl = Range {start = start old, end = moveCursor (end old) nl}

addNewMessage :: (String, Range) -> String -> String
addNewMessage (str, range) pre = pre ++ "\tError from " ++ show (start range) ++ " to " ++ show (end range) ++ " -> " ++ str ++ "\n"
