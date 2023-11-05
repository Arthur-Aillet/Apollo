{-
-- EPITECH PROJECT, 2023
-- Apollo
-- File description:
-- ParseInt
-}

module Parser.Int (module Parser.Int) where

import Control.Applicative (Alternative ((<|>)))
import Eval.Atom (Atom (..))
import Parser.Char (parseChar, parseDigit)
import Parser.Syntax (parseMany, parseSome)
import Parser.Type (Parser (..))

parseStringInt :: Parser String
parseStringInt = parseSome parseDigit

parseUInt :: Parser Atom
parseUInt = read <$> parseStringInt

parseNegInt :: Parser Atom
parseNegInt = (* (-1)) <$> (parseChar '-' *> parseUInt)

parseInt :: Parser Atom
parseInt = parseNegInt <|> parseUInt

parseUFloat :: Parser Atom
parseUFloat =
  (\intPart charPart -> read (intPart ++ charPart))
    <$> parseStringInt
    <*> parseFractionalPart

parseFractionalPart :: Parser String
parseFractionalPart = ('.' :) <$> (parseChar '.' *> parseMany parseDigit)

parseNegFloat :: Parser Atom
parseNegFloat = (* (-1)) <$> (parseChar '-' *> parseUFloat)

parseFloat :: Parser Atom
parseFloat = parseNegFloat <|> parseUFloat
