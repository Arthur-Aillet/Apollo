{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- BoolSpec.hs
-}

module Parser.BoolSpec (module Parser.BoolSpec) where

import Parser.Bool
import Parser.Position (defaultPosition)
import Parser.PositionSpec (getPosition)
import Parser.Range (defaultRange)
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Type (Parser (..))
import Test.HUnit

parseBoolTests :: Test
parseBoolTests =
  TestList
    [ "True" ~: (Right (True, ";", getPosition 4 0)) @=? (runParser parseBool "true;" defaultPosition),
      "False" ~: (Right (False, "   ;", getPosition 5 0)) @=? (runParser parseBool "false   ;" defaultPosition),
      "Empty list" ~: (Left (StackTrace [("Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser parseBool "" defaultPosition),
      "just #" ~: (Left (StackTrace [("Not Found: charactere is not 't' (is '#')", defaultRange, defaultLocation)])) @=? (runParser parseBool "#" defaultPosition)
    ]
