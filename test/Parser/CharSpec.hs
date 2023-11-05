{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- CharSpec.hs
-}

module Parser.CharSpec (charTests) where

import Parser.Char
import Parser.Position (defaultPosition)
import Parser.PositionSpec (getPosition)
import Parser.Range (defaultRange)
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Type (Parser (..))
import Test.HUnit

charTests :: Test
charTests =
  TestList
    [ "parseAChar" ~: parseACharTests,
      "parseDigit" ~: parseDigitTests,
      "parseOpeningAndClosing Functions" ~: parseOpeningAndClosingTests,
      "parseChar" ~: parseCharTests,
      "parseNotChar" ~: parseNotCharTests,
      "parseAnyChar" ~: parseAnyCharTests,
      "parseNotAnyChar" ~: parseNotAnyCharTests
    ]

parseACharTests :: Test
parseACharTests =
  TestList
    [ "delete the first character '\n'" ~: (Right ('\n', "hello world!", (getPosition 0 1))) @=? (runParser parseAChar "\nhello world!" defaultPosition),
      "delete the first character 'h'" ~: (Right ('h', "ello world!", (getPosition 1 0))) @=? (runParser (parseChar 'h') "hello world!" defaultPosition),
      "Error '\n'" ~: (Left (StackTrace [("Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser parseAChar "" defaultPosition)
    ]

parseDigitTests :: Test
parseDigitTests =
  TestList
    [ "is digit" ~: (Right ('9', "5678", (getPosition 1 0))) @=? (runParser parseDigit "95678" defaultPosition),
      "is not digit" ~: (Left (StackTrace [("Not Found: List is empty", defaultRange, defaultLocation)])) @=? (runParser parseDigit "foobar" defaultPosition),
      "Empty" ~: (Left (StackTrace [("Not Found: List is empty", defaultRange, defaultLocation)])) @=? (runParser parseDigit "foobar" defaultPosition)
    ]

parseOpeningAndClosingTests :: Test
parseOpeningAndClosingTests =
  TestList
    [ "Error parseOpeningQuote" ~: (Left (StackTrace [("Not Found: Missing opening Quote", defaultRange, defaultLocation)])) @=? (runParser parseOpeningQuote "foobar" defaultPosition),
      "Error parseClosingQuote" ~: (Left (StackTrace [("Not Found: Missing closing Quote", defaultRange, defaultLocation)])) @=? (runParser parseClosingQuote "foobar" defaultPosition),
      "Error parseOpeningsQuote" ~: (Left (StackTrace [("Not Found: Missing opening Quote", defaultRange, defaultLocation)])) @=? (runParser parseOpeningsQuote "foobar" defaultPosition),
      "Error parseClosingsQuote" ~: (Left (StackTrace [("Not Found: Missing closing Quote", defaultRange, defaultLocation)])) @=? (runParser parseClosingsQuote "foobar" defaultPosition),
      "Error parseOpeningParenthesis" ~: (Left (StackTrace [("Not Found: Missing opening Parenthesis", defaultRange, defaultLocation)])) @=? (runParser parseOpeningParenthesis "foobar" defaultPosition),
      "Error parseClosingParenthesis" ~: (Left (StackTrace [("Not Found: Missing closing Parenthesis", defaultRange, defaultLocation)])) @=? (runParser parseClosingParenthesis "foobar" defaultPosition),
      "Error parseOpeningCurlyBraquet" ~: (Left (StackTrace [("Not Found: Missing opening curlybraquet", defaultRange, defaultLocation)])) @=? (runParser parseOpeningCurlyBraquet "foobar" defaultPosition),
      "Error parseClosingCurlyBraquet" ~: (Left (StackTrace [("Not Found: Missing closing curlybraquet", defaultRange, defaultLocation)])) @=? (runParser parseClosingCurlyBraquet "foobar" defaultPosition),
      "Error parseOpeningBraquet" ~: (Left (StackTrace [("parseOpeningBraquet: Not Found: Missing opening braquet", defaultRange, defaultLocation)])) @=? (runParser parseOpeningBraquet "foobar" defaultPosition),
      "Error parseClosingBraquet" ~: (Left (StackTrace [("parseClosingBraquet: Not Found: Missing closing braquet", defaultRange, defaultLocation)])) @=? (runParser parseClosingBraquet "foobar" defaultPosition)
    ]

parseCharTests :: Test
parseCharTests =
  TestList
    [ "Test 1" ~: (Right ('\n', "hello world!", (getPosition 0 1))) @=? (runParser (parseChar '\n') "\nhello world!" defaultPosition),
      "Test 2" ~: (Right ('h', "ello world!", (getPosition 1 0))) @=? (runParser (parseChar 'h') "hello world!" defaultPosition),
      "Test 3" ~: (Left (StackTrace [("Not Found: char is not '\n' (is 'h')", defaultRange, defaultLocation)])) @=? (runParser (parseChar '\n') "hello world!" defaultPosition)
    ]

parseNotCharTests :: Test
parseNotCharTests =
  TestList
    [ "Test 1" ~: (Left (StackTrace [("Not Found: character is '\n'", defaultRange, defaultLocation)])) @=? (runParser (parseNotChar '\n') "\nhello world!" defaultPosition),
      "Test 2" ~: (Left (StackTrace [("Not Found: character is 'h'", defaultRange, defaultLocation)])) @=? (runParser (parseNotChar 'h') "hello world!" defaultPosition),
      "Test 3" ~: (Right ('h', "ello world!", getPosition 1 0)) @=? (runParser (parseNotChar '\n') "hello world!" defaultPosition)
    ]

parseAnyCharTests :: Test
parseAnyCharTests =
  TestList
    [ "Check if first char is in '\nc'" ~: (Right ('c', "oucou\n", (getPosition 1 0))) @=? (runParser (parseAnyChar "\nc") "coucou\n" defaultPosition),
      "Check if first char is in 'c\n'" ~: (Right ('c', "oucou\n", (getPosition 1 0))) @=? (runParser (parseAnyChar "c\n") "coucou\n" defaultPosition),
      "Check if first char is in 'rem'" ~: (Left (StackTrace [("Not Found: List is empty", defaultRange, defaultLocation)])) @=? (runParser (parseAnyChar "rem") "zero\n" defaultPosition),
      "Check if first char is in ''" ~: (Left (StackTrace [("Not Found: List is empty", defaultRange, defaultLocation)])) @=? (runParser (parseAnyChar "") "zero\n" defaultPosition)
    ]

parseNotAnyCharTests :: Test
parseNotAnyCharTests =
  TestList
    [ "Not any char of the string" ~: (Right ('f', "oo\n", (getPosition 1 0))) @=? (runParser (parseNotAnyChar "abcd") "foo\n" defaultPosition),
      "Is in the string" ~: (Left (StackTrace [("Not Found: character is 'b'", defaultRange, defaultLocation)])) @=? (runParser (parseNotAnyChar "abcd") "bar" defaultPosition),
      "String is empty" ~: (Left (StackTrace [("Not Found: List is empty", defaultRange, defaultLocation)])) @=? (runParser (parseNotAnyChar "") "zero\n" defaultPosition)
    ]
