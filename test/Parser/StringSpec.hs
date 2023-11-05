{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- StringSpec.hs
-}

module Parser.StringSpec (stringTests) where

import Parser.String

import Parser.Position (defaultPosition)
import Parser.PositionSpec (getPosition)
import Parser.Type (Parser (..))
import Test.HUnit

stringTests :: Test
stringTests =
    TestList
    ["parseStr" ~: parseStrTests
    ]

parseStrTests :: Test
parseStrTests =
    TestList
    [ "OpVariable" ~: (Right ([], "\"hello\"", getPosition 0 0)) @=? (runParser parseStr "\"hello\"" defaultPosition)
    ]