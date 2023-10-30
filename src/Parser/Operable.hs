{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Operable.hs
-}

module Parser.Operable (module Parser.Operable) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Type(Parser(..))
import Eval.Atom (Atom(..))
import Ast.Type(Type(..), Operable(..))
import Parser.Symbol(parseSymbol, parseType)
import Parser.Int(parseInt, parseFloat)
import Parser.Bool(parseBool)
import Parser.Syntax(parseWithSpace, parseMany)
import Parser.Char(parseAnyChar)

parseDefinitionName :: Parser String
parseDefinitionName = parseWithSpace (parseMany (parseAnyChar (['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_-")))

parseAtom :: Parser Atom
parseAtom = parseFloat <|> parseInt <|> (AtomB <$> parseBool)

parseCast:: Parser Type
parseCast = parseWithSpace (parseSymbol "as" *> (parseWithSpace parseType))

parseOp :: Parser Operable
parseOp = (OpValue <$> (parseWithSpace parseAtom)) <|> (OpVariable <$> (parseWithSpace  parseDefinitionName))

parseOperable :: Parser Operable
parseOperable = (OpCast <$> parseOp <*> parseCast) <|> parseOp
