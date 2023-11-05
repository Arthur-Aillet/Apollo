{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Position.hs
-}

module Parser.PositionSpec (module Parser.PositionSpec) where

import Test.HUnit
import Parser.Position (Position (..), defaultPosition, moveCursor)
import Parser.StackTrace (SourceLocation(..))

positionTests :: Test
positionTests =
  TestList
  [ "defaultPosition" ~: defaultPositionTest,
    "moveCursor" ~: moveCursorTests
  ]

getPosition :: Int -> Int -> Position
getPosition x y = Position {line = y, char = x}

getRangePosition :: Int -> Int -> SourceLocation
getRangePosition x y = SourceLocation {functionName = "", fileName = "", l = y, c = x}

defaultPositionTest :: Test
defaultPositionTest = TestCase $ assertEqual "default position is line = 0, char = 0" (getPosition 0 0) defaultPosition

moveCursorTests :: Test
moveCursorTests =
  TestList
    [ "Move cursor to the next char" ~: (getPosition 0 1) @=? (moveCursor (getPosition 0 0) True),
      "Move cursor to the next line" ~: (getPosition 1 0) @=? (moveCursor (getPosition 0 0) False)
    ]
