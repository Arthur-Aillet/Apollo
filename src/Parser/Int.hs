{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseInt
-}

module Parser.Int (module Parser.Int) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Char (parseChar, parseDigit)
import Parser.Type (Parser (..))
import Parser.Syntax (parseMany, parseSome)
import Eval.Atom (Atom (..))
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
