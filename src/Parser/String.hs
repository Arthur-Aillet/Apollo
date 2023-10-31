--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- String
--

module Parser.String (module Parser.String) where

-- import Control.Applicative (Alternative ((<|>)))
-- import Parser.Char (parseChar, parseDigit, parseAnyChar)
-- import Parser.StackTrace (StackTrace (..))
-- import Parser.Type (Parser (..))
-- import Parser.Syntax (parseMany, parseSome)
-- import Parser.Range (Range (..))

-- acceptableCharacters :: [Char]
-- acceptableCharacters = ['a'..'z']
--                     ++ ['A'..'Z']
--                     ++ ['0'..'9']
--                     ++ ['|', '/', '\\', '[', ']', '(', ')', '{', '}', '-', '_', '\"', '\'']

-- parseString :: Parser String
-- parseString = parseStringQuote <|> parseStringSingleQuote

-- -- parseStringWithHandleBackslash :: Parser String
-- -- parseStringWithHandleBackslash = Parser $ \s p -> case runParser (parseAnyChar acceptableCharacters) s p of
-- --     Right (element, string, pos) ->
-- --         case runParser (parseStringWithHandleBackslash $ parseAnyChar acceptableCharacters) string pos of
-- --             Left _ -> Right ([], string, pos)
-- --             Right ('\\', x : str, new_pos) -> Right (element : x, str, new_pos + 1)
-- --             Right (found, str, new_pos) -> Right (element : found, str, new_pos)
-- --     Left a -> Left a

-- -- parseStringSingleQuote :: Parser String
-- -- parseStringSingleQuote = Parser $ \s p -> parseChar '\'' *> parseSome $ parseAnyChar acceptableCharacters  <* parseChar '\''

-- parseStringQuote :: Parser String
-- parseStringQuote =  parseChar '\"' *> parseSome parseDigit <* parseChar '\"'
