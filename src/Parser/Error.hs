{-
-- EPITECH PROJECT, 2023
-- Apollo
-- File description:
-- ParseError
-}

module Parser.Error (module Parser.Error) where

import Parser.Position (Position (..))
import Parser.Range (Range (..))
import Parser.StackTrace (defaultLocation)
import Parser.Type (Parser (..), StackTrace (..))

withErr :: String -> Parser a -> Parser a
withErr new_msg parser = Parser $ \str pos -> case runParser parser str pos of
  Right a -> Right a
  Left (StackTrace ((msg, Range start end, src) : xs)) ->
    Left
      ( StackTrace
          ((new_msg, Range start end, src) : (msg, Range start end, src) : xs)
      )
  Left err -> Left err

replaceErr :: String -> Parser a -> Parser a
replaceErr new_msg parser = Parser $ \str pos ->
  case runParser parser str pos of
    Right a -> Right a
    Left (StackTrace ((_, Range start end, src) : xs)) ->
      Left (StackTrace ((new_msg, Range start end, src) : xs))
    Left err -> Left err

failingWith :: String -> Parser a
failingWith string =
  Parser
    ( \_ pos ->
        Left (StackTrace [(string, Range pos pos, defaultLocation)])
    )

printErr :: (String, Position) -> IO ()
printErr (err, pos) =
  putStrLn
    ( "Error found at "
        ++ show (line pos)
        ++ ":"
        ++ show (char pos)
        ++ ":"
        ++ "\n    "
        ++ err
    )