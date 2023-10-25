module ParserSpec (parserTests) where

import Parser.Char
import Parser.Position (Position (..), defaultPosition, moveCursor)
import Parser.Range (defaultRange)
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Symbol
import Parser.Syntax
import Parser.Type (Parser (..))
import Test.HUnit

parserTests :: Test
parserTests =
  TestList
    [ "defaultPosition" ~: defaultPositionTest,
      "moveCursor" ~: moveCursorTests,
      -- "parseBool" ~: parseBoolTests,
      "parseAChar" ~: parseACharTests,
      "parseChar" ~: parseCharTests,
      "parseNotChar" ~: parseNotCharTests,
      "parseAnyChar" ~: parseAnyCharTests,
      -- "parseString" ~: parseStringTests,
      "parseSymbol" ~: parseSymbolTests,
      -- "parseOr" ~: parseOrTests,
      -- "parseAnd" ~: parseAndTests,
      "parseMany" ~: parseManyTests
      -- "parseSome" ~: parseSomeTests,
      -- "parseDigit" ~: parseDigitTests,
      -- "parseUInt" ~: parseUIntTests,
      -- "parseNegInt" ~: parseNegIntTests,
      -- "parseInt" ~: parseIntTests,
      -- "parseFloat" ~: parseFloatTest,
      -- "parseWithSpace" ~: parseWithSpaceTests,
      -- "parseOpeningParenthesis" ~: parseOpeningParenthesisTests,
      -- "parseClosingParenthesis" ~: parseClosingParenthesisTests,
      -- "parseOpeningQuote" ~: parseOpeningQuoteTests,
      -- "parseClosingQuote" ~: parseClosingQuoteTests
    ]

getPosition :: Int -> Int -> Position
getPosition x y = Position {line = y, char = x}

defaultPositionTest :: Test
defaultPositionTest = TestCase $ assertEqual "default position is line = 0, char = 0" (getPosition 0 0) defaultPosition

moveCursorTests :: Test
moveCursorTests =
  TestList
    [ "Move cursor to the next char" ~: (getPosition 0 1) @=? (moveCursor (getPosition 0 0) True),
      "Move cursor to the next line" ~: (getPosition 1 0) @=? (moveCursor (getPosition 0 0) False)
    ]

parseACharTests :: Test
parseACharTests =
  TestList
    [ "delete the first character '\n'" ~: (Right ('\n', "hello world!", (getPosition 0 1))) @=? (runParser parseAChar "\nhello world!" defaultPosition),
      "delete the first character 'h'" ~: (Right ('h', "ello world!", (getPosition 1 0))) @=? (runParser (parseChar 'h') "hello world!" defaultPosition),
      "Error '\n'" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser parseAChar "" defaultPosition)
    ]

parseCharTests :: Test
parseCharTests =
  TestList
    [ "Test 1" ~: (Right ('\n', "hello world!", (getPosition 0 1))) @=? (runParser (parseChar '\n') "\nhello world!" defaultPosition),
      "Test 2" ~: (Right ('h', "ello world!", (getPosition 1 0))) @=? (runParser (parseChar 'h') "hello world!" defaultPosition),
      "Test 3" ~: (Left (StackTrace [("parseChar: Not Found: charactere is not '\n' (is 'h')", defaultRange, defaultLocation)])) @=? (runParser (parseChar '\n') "hello world!" defaultPosition)
    ]

parseNotCharTests :: Test
parseNotCharTests =
  TestList
    [ "Test 1" ~: (Left (StackTrace [("parseNotChar: Not Found: character is '\n'", defaultRange, defaultLocation)])) @=? (runParser (parseNotChar '\n') "\nhello world!" defaultPosition),
      "Test 2" ~: (Left (StackTrace [("parseNotChar: Not Found: character is 'h'", defaultRange, defaultLocation)])) @=? (runParser (parseNotChar 'h') "hello world!" defaultPosition),
      "Test 3" ~: (Right ('h', "ello world!", getPosition 1 0)) @=? (runParser (parseNotChar '\n') "hello world!" defaultPosition)
    ]

parseAnyCharTests :: Test
parseAnyCharTests =
  TestList
    [ "check if first char is in '\nc'" ~: (Right ('c', "oucou\n", (getPosition 1 0))) @=? (runParser (parseAnyChar "\nc") "coucou\n" defaultPosition),
      "check if first char is in 'c\n'" ~: (Right ('c', "oucou\n", (getPosition 1 0))) @=? (runParser (parseAnyChar "c\n") "coucou\n" defaultPosition),
      "check if first char is in 'rem'" ~: (Left (StackTrace [("Not Found: List is empty", defaultRange, defaultLocation)])) @=? (runParser (parseAnyChar "rem") "zero\n" defaultPosition),
      "check if first char is in ''" ~: (Left (StackTrace [("Not Found: List is empty", defaultRange, defaultLocation)])) @=? (runParser (parseAnyChar "") "zero\n" defaultPosition)
    ]

-- parseBoolTests :: Test
-- parseBoolTests =
--   TestList
--     [ "True" ~: (Right (True, " foo", getPosition 2 0)) @=? (runParser parseBool "#t foo" defaultPosition),
--       "False" ~: (Right (False, "", getPosition 2 0)) @=? (runParser parseBool "#f" defaultPosition),
--       "Empty list" ~: (Left (StackTrace [("Error: parseChar: Not Found: charactere is not 'T' (is '#') at 0:0:0:0", defaultRange, defaultLocation)])) @=? (runParser parseBool "" defaultPosition),
--       "just #" ~: (Left (StackTrace [("Error: parseChar: Not Found: charactere is not 'T' (is '#') at 0:0:0:0", defaultRange, defaultLocation)])) @=? (runParser parseBool "#" defaultPosition)
--     ]

parseSymbolTests :: Test
parseSymbolTests =
  TestList
    [ "Test 1" ~: (Right ("azerty", " hello", (getPosition 6 0))) @=? (runParser (parseSymbol "azerty") "azerty hello" defaultPosition),
      "Test 2" ~: (Left (StackTrace [("parseChar: Not Found: charactere is not 'e' (is 'a')", defaultRange, defaultLocation)])) @=? (runParser (parseSymbol "ezryta") "azerty hello" defaultPosition),
      "Test 3" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser (parseSymbol "azerty") "" defaultPosition)
    ]

-- parseStringTests :: Test
-- parseStringTests =
--   TestList
--     [ "Test 1" ~: (Right ("!world hello", "", getPosition 14 0)) @=? (runParser parseString "\"!world hello\"" defaultPosition),
--       "Test 2" ~: (Left ("Not Found: Missing opening Quote", (getPosition 0 0))) @=? (runParser parseString "" defaultPosition),
--       "Test 3" ~: (Left ("Not Found: Missing opening Quote", (getPosition 1 0))) @=? (runParser parseString "foo" defaultPosition),
--       "Test 4" ~: (Left ("Not Found: Missing closing Quote", (getPosition 4 0))) @=? (runParser parseString "\"bar" defaultPosition)
--     ]

-- parseOrTests :: Test
-- parseOrTests =
--   TestList
--     [ "Test 1" ~: (Right ('(', "hello world)", (getPosition 1 0))) @=? (runParser (parseOr (parseChar '(') (parseChar 'f')) "(hello world)" defaultPosition),
--       "Test 2" ~: (Right ('(', "hello world)", (getPosition 1 0))) @=? (runParser (parseOr (parseChar 'f') (parseChar '(')) "(hello world)" defaultPosition),
--       "Test 3" ~: (Right ('(', "hello world)", (getPosition 1 0))) @=? (runParser (parseOr (parseChar '(') (parseChar '(')) "(hello world)" defaultPosition),
--       "Test 4" ~: (Left ("Not Found: charactere is not 'f'", (getPosition 1 0))) @=? (runParser (parseOr (parseChar 'f') (parseChar 'f')) "(hello world)" defaultPosition)
--     ]

-- parseAndTests :: Test
-- parseAndTests =
--   TestList
--     [ "Test 1" ~: (Left ("Not Found: charactere is not 'f'", (getPosition 2 0))) @=? (runParser (parseAnd (parseChar '(') (parseChar 'f')) "(hello world)" defaultPosition),
--       "Test 2" ~: (Left ("Not Found: charactere is not '('", (getPosition 2 0))) @=? (runParser (parseAnd (parseChar '(') (parseChar '(')) "(hello world)" defaultPosition),
--       "Test 3" ~: (Right (('(', 'h'), "ello world)", Position {line = 0, char = 2})) @=? (runParser (parseAnd (parseChar '(') (parseChar 'h')) "(hello world)" defaultPosition),
--       "Test 4" ~: (Left ("Not Found: charactere is not 'h'", Position {line = 0, char = 1})) @=? (runParser (parseAnd (parseChar 'h') (parseChar '(')) "(hello world)" defaultPosition)
    -- ]

parseManyTests :: Test
parseManyTests =
  TestList
    [ "Test 1" ~: (Right ("      ", "hello world!", (getPosition 6 0))) @=? (runParser (parseMany (parseChar ' ')) "      hello world!" defaultPosition),
      "Test 2" ~: (Left (StackTrace [("parseChar: Not Found: charactere is not ' ' (is 'h')", defaultRange, defaultLocation)])) @=? (runParser (parseMany (parseChar ' ')) "hello world!" defaultPosition),
      "Test 3" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser (parseMany (parseChar ' ')) "" defaultPosition)
    ]

-- parseSomeTests :: Test
-- parseSomeTests =
--   TestList
--     [ "Test 1" ~: (Right ("      ", "hello world!", (getPosition 6 0))) @=? (runParser (parseSome (parseChar ' ')) "      hello world!" defaultPosition),
--       "Test 2" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser (parseSome (parseChar ' ')) "hello world!" defaultPosition),
--       "Test 3" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser (parseSome (parseChar ' ')) "" defaultPosition)
--     ]

-- parseDigitTests :: Test
-- parseDigitTests =
--   TestList
--     [ "Test 1" ~: (Right ('6', " 5 3 7 4 4", (getPosition 1 0))) @=? (runParser parseDigit "6 5 3 7 4 4" defaultPosition),
--       "Test 1" ~: (Right ('6', "66 == devil", (getPosition 1 0))) @=? (runParser parseDigit "666 == devil" defaultPosition),
--       "Test 3" ~: (Left ("Not Found: List is empty", (getPosition 0 0))) @=? (runParser parseDigit "" defaultPosition),
--       "Test 4" ~: (Left ("Not Found: List is empty", (getPosition 0 0))) @=? (runParser parseDigit " 666" defaultPosition)
--     ]

-- parseUIntTests :: Test
-- parseUIntTests =
--   TestList
--     [ "Test 1" ~: (Right (6, " 5 3 7 4 4", (getPosition 1 0))) @=? (runParser parseUInt "6 5 3 7 4 4" defaultPosition),
--       "Test 2" ~: (Right (653744, "", (getPosition 6 0))) @=? (runParser parseUInt "653744" defaultPosition),
--       "Test 3" ~: (Left ("Not Found: List is empty", (getPosition 0 0))) @=? (runParser parseUInt "foo bar" defaultPosition),
--       "Test 4" ~: (Left ("Not Found: List is empty", (getPosition 0 0))) @=? (runParser parseUInt "-6" defaultPosition)
--     ]

-- parseNegIntTests :: Test
-- parseNegIntTests =
--   TestList
--     [ "Test 1" ~: (Right (-6, " 5 3 7 4 4", (getPosition 2 0))) @=? (runParser parseNegInt "-6 5 3 7 4 4" defaultPosition),
--       "Test 2" ~: (Left ("Not Found: charactere is not '-'", (getPosition 1 0))) @=? (runParser parseNegInt "6 5 3 7 4 4" defaultPosition),
--       "Test 3" ~: (Left ("Not Found: List is empty", (getPosition 0 0))) @=? (runParser parseUInt "foo bar" defaultPosition)
--     ]

-- parseIntTests :: Test
-- parseIntTests =
--   TestList
--     [ "Test 1" ~: (Right (6, " 5 3 7 4 4", (getPosition 1 0))) @=? (runParser parseUInt "6 5 3 7 4 4" defaultPosition),
--       "Test 2" ~: (Right (653744, "", (getPosition 6 0))) @=? (runParser parseUInt "653744" defaultPosition),
--       "Test 3" ~: (Right (-6, " 5 3 7 4 4", (getPosition 2 0))) @=? (runParser parseNegInt "-6 5 3 7 4 4" defaultPosition),
--       "Test 4" ~: (Left ("Not Found: List is empty", (getPosition 0 0))) @=? (runParser parseUInt "foo bar" defaultPosition)
--     ]

-- parseFloatTest :: Test
-- parseFloatTest =
--   TestList
--     [ "Test 1" ~: (Right (123.123, "", getPosition 7 0)) @=? (runParser parseFloat "123.123" defaultPosition),
--       "Test 2" ~: (Right (-123.123, "", getPosition 8 0)) @=? (runParser parseFloat "-123.123" defaultPosition),
--       "Test 3" ~: (Left ("Not Found: charactere is not '-'", (getPosition 1 0))) @=? (runParser parseFloat "123" defaultPosition),
--       "Test 4" ~: (Left ("Not Found: List is empty", (getPosition 0 0))) @=? (runParser parseFloat "" defaultPosition)
--     ]

-- parseWithSpaceTests :: Test
-- parseWithSpaceTests =
--   TestList
--     [ "Test 1" ~: (Right ('h', "ello world!", (getPosition 6 0))) @=? (runParser (parseWithSpace (parseChar 'h')) "     hello world!" defaultPosition),
--       "Test 2" ~: (Right ('h', "ello world!", (getPosition 1 0))) @=? (runParser (parseWithSpace (parseChar 'h')) "hello world!" defaultPosition),
--       "Test 3" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser (parseWithSpace (parseChar '\n')) "hello world!" defaultPosition)
--     ]

-- parseOpeningParenthesisTests :: Test
-- parseOpeningParenthesisTests =
--   TestList
--     [ "Test 1" ~: (Right ('(', "hello world!)", (getPosition 1 0))) @=? (runParser parseOpeningParenthesis "(hello world!)" defaultPosition),
--       "Test 2" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser parseOpeningParenthesis "hello world!)" defaultPosition)
--     ]

-- parseClosingParenthesisTests :: Test
-- parseClosingParenthesisTests =
--   TestList
--     [ "Test 1" ~: (Right (')', "", (getPosition 1 0))) @=? (runParser parseClosingParenthesis ")" defaultPosition),
--       "Test 2" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser parseClosingParenthesis "(hello world!)" defaultPosition)
--     ]

-- parseOpeningQuoteTests :: Test
-- parseOpeningQuoteTests =
--   TestList
--     [ "Test 1" ~: (Right ('\"', "hello world!\"", (getPosition 1 0))) @=? (runParser parseOpeningQuote "\"hello world!\"" defaultPosition),
--       "Test 2" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser parseOpeningQuote "hello world!\"" defaultPosition)
--     ]

-- parseClosingQuoteTests :: Test
-- parseClosingQuoteTests =
--   TestList
--     [ "Test 1" ~: (Right ('\"', "", (getPosition 1 0))) @=? (runParser parseClosingQuote "\"" defaultPosition),
--       "Test 2" ~: (Left (StackTrace [("parseAChar: Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser parseClosingQuote "(hello world!)" defaultPosition)
--     ]
