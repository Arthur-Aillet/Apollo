{-
-- EPITECH PROJECT, 2023
-- Dev_repo
-- File description:
-- ParseError
-}

module Parser.Error (module Parser.Error) where

import Parser.Type (Parser (..), StackTrace(..), defaultRange)
import Parser.Position (Position (..), Range(..), newRange)

withErr :: String -> Parser a -> Parser a
withErr new_msg parser = Parser $ \string pos -> case runParser parser string pos of
  Right a -> Right a
  Left (StackTrace ((msg, old_range):xs)) -> Left StackTrace { errors = (new_msg, Range { start = pos, end = end old_range}) : (msg, old_range) : xs}
  Left err -> Left err

failingWith :: String -> Parser a
failingWith string = Parser (\_ pos -> Left StackTrace { errors = [(string, newRange pos pos)] })

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
