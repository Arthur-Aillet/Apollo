{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseError
-}

module Parser.Error (module Parser.Error) where

import Parser.Type (Parser (..))
import Parser.Position (Position (..))

withErr :: String -> Parser a -> Parser a
withErr msg parser = Parser $ \string pos -> case runParser parser string pos of
  Right a -> Right a
  Left (_, new_pos) -> Left (msg, new_pos)

failingWith :: String -> Parser a
failingWith string = Parser (\_ pos -> Left (string, pos))

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
