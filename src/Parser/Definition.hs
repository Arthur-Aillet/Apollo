{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Definition.hs
-}

module Parser.Definition (module Parser.Definition) where

import Ast.Type (Ast (..), Definition (..), Function (..), Operable (..), Type (..))
import Control.Applicative (Alternative ((<|>)))
import Data.Tuple (swap)
import Parser.Char (parseChar, parseClosingCurlyBraquet, parseClosingParenthesis, parseOpeningCurlyBraquet, parseOpeningParenthesis)
import Parser.Operable (parseDefinitionName)
import Parser.Symbol (parseMaybeType, parseType)
import Parser.Syntax (parseMany, parseWithSpace)
import Parser.Type (Parser (..))
import Parser.StackTrace (StackTrace(..), addSourceLocation, modifySourceLocation)
import Parser.Error(replaceErr)
import Parser.Structure

parseParameter :: Parser (String, Type)
parseParameter =
  (swap <$> ((,) <$> typ <*> str))
  where
    typ = parseType
    str = parseDefinitionName

parseParameterWithComa :: Parser (String, Type)
parseParameterWithComa = parseParameter <* parseChar ','

parseParameters :: Parser [(String, Type)]
parseParameters =
  parseWithSpace
    ( parseOpeningParenthesis
        *> parseMany (parseWithSpace (parseParameterWithComa <|> parseParameter))
        <* parseClosingParenthesis
    )

parseInstruction :: Parser Ast
parseInstruction = (AstStructure <$> parseSequence)

parseInstructions :: Parser Ast
parseInstructions =
  parseWithSpace
    ( parseOpeningCurlyBraquet
        *>  parseWithSpace parseInstruction
        <* parseClosingCurlyBraquet
    )

parseFunction :: Parser Function
parseFunction = Function <$> parseParameters <*> parseMaybeType <*> parseInstructions

parseFuncDefinition :: Parser Definition
parseFuncDefinition = Parser $ \s p -> case runParser (replaceErr "Synatxe error: bad function definition" ((parseChar '@') *> parseDefinitionName)) s p of
  Right (name, str, pos) -> case runParser parseFunction str pos of
    Right (func, string, position) -> Right ((FuncDefinition name func), string, position)
    Left (StackTrace a) -> Left (StackTrace (modifySourceLocation (addSourceLocation name p) a))
  Left a -> Left a


  -- FuncDefinition <$> ((parseChar '@') *> parseDefinitionName) <*> parseFunction
