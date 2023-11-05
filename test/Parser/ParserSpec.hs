module Parser.ParserSpec (parserTests) where

import Parser.BoolSpec
import Parser.CharSpec
import Parser.PositionSpec
import Parser.SymbolSpec
import Parser.DefinitionSpec
import Parser.StringSpec
import Test.HUnit

parserTests :: Test
parserTests =
  TestList
    [ positionTests,
      charTests,
      symbolTests,
      definitionTests,
      stringTests,
      "parseBool" ~: parseBoolTests
    ]
