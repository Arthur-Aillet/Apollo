{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- SymbolSpec.hs
-}

module Parser.SymbolSpec (symbolTests) where

import Ast.Ast (Type (..))
import Parser.Position (defaultPosition)
import Parser.PositionSpec (getPosition)
import Parser.Range (Range (..), defaultRange)
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Symbol
import Parser.Type (Parser (..))
import Test.HUnit

symbolTests :: Test
symbolTests =
  TestList
    [ "parseSymbol" ~: parseSymbolTests,
      "goodType" ~: goodTypeTests,
      "parseListType" ~: parseListTypeTests,
      "parseSymbolType" ~: parseSymbolTypeTests,
      "parseType" ~: parseTypeTests,
      "parseMaybeType" ~: parseMaybeTypeTests
    ]

parseSymbolTests :: Test
parseSymbolTests =
  TestList
    [ "Test 1" ~: (Right ("azerty", " hello", (getPosition 6 0))) @=? (runParser (parseSymbol "azerty") "azerty hello" defaultPosition),
      "Test 2" ~: (Left (StackTrace [("Not Found: char is not 'e' (is 'a')", defaultRange, defaultLocation)])) @=? (runParser (parseSymbol "ezryta") "azerty hello" defaultPosition),
      "Test 3" ~: (Left (StackTrace [("Not Found: End of Input", defaultRange, defaultLocation)])) @=? (runParser (parseSymbol "azerty") "" defaultPosition)
    ]

goodTypeTests :: Test
goodTypeTests =
  TestList
    [ "Type valide (TypeInt)" ~: (Right (TypeInt, "int", defaultPosition)) @=? (runParser (isgoodType (pure (Just (TypeInt)))) "int" defaultPosition),
      "Type valide (TypeString)" ~: (Right (TypeString, "string", defaultPosition)) @=? (runParser (isgoodType (pure (Just TypeString))) "string" defaultPosition),
      "Type valide (TypeBool)" ~: (Right (TypeBool, "bool", defaultPosition)) @=? (runParser (isgoodType (pure (Just TypeBool))) "bool" defaultPosition),
      "Type valide (TypeChar)" ~: (Right (TypeChar, "char", defaultPosition)) @=? (runParser (isgoodType (pure (Just TypeChar))) "char" defaultPosition),
      "Type valide (TypeList TypeInt)" ~: (Right (TypeList (Just TypeInt), "[int]", defaultPosition)) @=? (runParser (isgoodType (pure (Just (TypeList (Just TypeInt))))) "[int]" defaultPosition),
      "Type valide (TypeList TypeString)" ~: (Right (TypeList (Just TypeString), "[string]", defaultPosition)) @=? (runParser (isgoodType (pure (Just (TypeList (Just TypeString))))) "[string]" defaultPosition),
      "Type valide (TypeList (TypeList TypeChar))" ~: (Right (TypeList (Just (TypeList (Just TypeChar))), "[[char]]", defaultPosition)) @=? (runParser (isgoodType (pure (Just (TypeList (Just (TypeList (Just TypeChar))))))) "[[char]]" defaultPosition),
      "Type invalide (Renvoie Nothing)" ~: (Left (StackTrace [("This type doesn't exist: ", defaultRange, defaultLocation)])) @=? (runParser (isgoodType (pure Nothing)) "invalid" defaultPosition)
    ]

parseListTypeTests :: Test
parseListTypeTests =
  TestList
    [ "TypeList (TypeInt)" ~: (Right ("[int]", "", getPosition 5 0)) @=? (runParser parseListType "[int]" defaultPosition),
      "TypeList (TypeString)" ~: (Right ("[string]", "", getPosition 8 0)) @=? (runParser parseListType "[string]" defaultPosition),
      "TypeList (TypeBool)" ~: (Right ("[bool]", "", getPosition 6 0)) @=? (runParser parseListType "[bool]" defaultPosition),
      "Type invalide (TypeList)" ~: (Left (StackTrace [("Not Found: char is not 't' (is 'v')", Range (getPosition 3 0) (getPosition 3 0), defaultLocation)])) @=? (runParser parseListType "[invalid]" defaultPosition),
      "Type invalide (Symbole manquant)" ~: (Left (StackTrace [("parseClosingBraquet: Not Found: Missing closing braquet", Range (getPosition 4 0) (getPosition 4 0), defaultLocation)])) @=? (runParser parseListType "[int" defaultPosition)
    ]

parseSymbolTypeTests :: Test
parseSymbolTypeTests =
  TestList
    [ "Type valide (TypeInt)" ~: (Right ("int", "", getPosition 3 0)) @=? (runParser parseSymbolType "int" defaultPosition),
      "Type valide (TypeString)" ~: (Right ("string", "", getPosition 6 0)) @=? (runParser parseSymbolType "string" defaultPosition),
      "Type valide (TypeBool)" ~: (Right ("bool", "", getPosition 4 0)) @=? (runParser parseSymbolType "bool" defaultPosition),
      "Type valide (TypeChar)" ~: (Right ("char", "", getPosition 4 0)) @=? (runParser parseSymbolType "char" defaultPosition),
      "Type valide (TypeList)" ~: (Right ("[int]", "", getPosition 5 0)) @=? (runParser parseSymbolType "[int]" defaultPosition),
      "Type invalide" ~: (Left (StackTrace [("Not Found: char is not 't' (is 'v')", Range (getPosition 2 0) (getPosition 2 0), defaultLocation)])) @=? (runParser parseSymbolType "invalid" defaultPosition)
    ]

parseTypeTests :: Test
parseTypeTests =
  TestList
    [ "Type valide (TypeInt)" ~: (Right (TypeInt, "", getPosition 3 0)) @=? (runParser parseType "int" defaultPosition),
      "Type valide (TypeString)" ~: (Right (TypeList (Just TypeChar), "", getPosition 6 0)) @=? (runParser parseType "string" defaultPosition),
      "Type valide (TypeBool)" ~: (Right (TypeBool, "", getPosition 4 0)) @=? (runParser parseType "bool" defaultPosition),
      "Type valide (TypeChar)" ~: (Right (TypeChar, "", getPosition 4 0)) @=? (runParser parseType "char" defaultPosition),
      "Type valide (TypeList)" ~: (Right ((TypeList (Just TypeInt)), "", getPosition 5 0)) @=? (runParser parseType "[int]" defaultPosition),
      "Type invalide" ~: (Left (StackTrace [("Not Found: char is not 't' (is 'v')", Range (getPosition 2 0) (getPosition 2 0), defaultLocation)])) @=? (runParser parseType "invalid" defaultPosition)
    ]

parseMaybeTypeTests :: Test
parseMaybeTypeTests =
  TestList
    [ "Type valide (TypeInt)" ~: (Right (Just TypeInt, "", getPosition 3 0)) @=? (runParser parseMaybeType "int" defaultPosition),
      "Type valide (TypeString)" ~: (Right (Just (TypeList (Just TypeChar)), "", getPosition 6 0)) @=? (runParser parseMaybeType "string" defaultPosition),
      "Type valide (TypeBool)" ~: (Right (Just TypeBool, "", getPosition 4 0)) @=? (runParser parseMaybeType "bool" defaultPosition),
      "Type valide (TypeChar)" ~: (Right (Just TypeChar, "", getPosition 4 0)) @=? (runParser parseMaybeType "char" defaultPosition),
      "Type valide (TypeList)" ~: (Right (Just (TypeList (Just TypeInt)), "", getPosition 5 0)) @=? (runParser parseMaybeType "[int]" defaultPosition),
      "Type invalide" ~: (Right (Nothing, "invalid", defaultPosition)) @=? (runParser parseMaybeType "invalid" defaultPosition)
    ]
