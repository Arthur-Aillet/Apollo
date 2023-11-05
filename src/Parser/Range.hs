{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module Parser.Range (Range (..), defaultRange, growRange, addNewMessage) where

import Parser.Position (Position (..), defaultPosition, moveCursor)

data Range = Range Position Position deriving (Show, Eq)

defaultRange :: Range
defaultRange = Range defaultPosition defaultPosition

growRange :: Range -> Bool -> Range
growRange (Range start end) nl = Range start (moveCursor end nl)

addNewMessage :: (String, Range) -> String -> String
addNewMessage (str, Range start end) pre =
  pre
    ++ "\tError from "
    ++ show start
    ++ " to "
    ++ show end
    ++ " -> "
    ++ str
    ++ "\n"