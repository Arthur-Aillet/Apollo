{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Operable.hs
-}

module Parser.Operable (module Parser.Operable) where

import Ast.Type (Operable (..), Type (..))
import Control.Applicative (Alternative ((<|>)))
import Eval.Atom (Atom (..))
import Parser.Bool (parseBool)
import Parser.Char (parseAnyChar)
import Parser.Int (parseFloat, parseInt)
import Parser.Symbol (parseSymbol, parseType)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))

parseDefinitionName :: Parser String
parseDefinitionName = parseWithSpace (parseMany (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_-")))

parseAtom :: Parser Atom
parseAtom = parseFloat <|> parseInt <|> (AtomB <$> parseBool)

parseCast :: Parser Type
parseCast = parseWithSpace (parseSymbol "as" *> (parseWithSpace parseType))

parseOp :: Parser Operable
parseOp = (OpValue <$> (parseWithSpace parseAtom)) <|> (OpVariable <$> (parseWithSpace parseDefinitionName))

parseOperable :: Parser Operable
parseOperable = (OpCast <$> parseOp <*> parseCast) <|> parseOp
