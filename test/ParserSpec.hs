module ParserSpec (parserTests) where

import Test.HUnit

parserTests :: Test
parserTests =
  TestList
    [ "example" ~: exampleTests
    ]

exampleTests :: Test
exampleTests =
  TestList
    [ "Test 1" ~: True @=? True
    ]
