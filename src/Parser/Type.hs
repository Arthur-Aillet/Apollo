{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Parser type
-}

module Parser.Type (Parser (..), StackTrace (..), defaultRange, faillingParser) where

import Control.Applicative (Alternative ((<|>), empty))
import Parser.Position (Position (..))
import Parser.Range (defaultRange)
import Parser.StackTrace (StackTrace (..))

newtype Parser a = Parser
  { runParser ::
      String ->
      Position ->
      Either StackTrace (a, String, Position)
  }

faillingParser :: Parser a
faillingParser = Parser $ \_ _ -> Left $ StackTrace []

instance Functor Parser where
  fmap fct parser = Parser $ \string pos -> case runParser parser string pos of
    Right (a, new_string, new_pos) -> Right (fct a, new_string, new_pos)
    Left a -> Left a

instance Applicative Parser where
  pure a = Parser $ \string pos -> Right (a, string, pos)
  (<*>) parserfct parsera = Parser $ \s pos -> case runParser parserfct s pos of
    Right (fct, new_s, new_pos) -> case runParser parsera new_s new_pos of
      Right (a, snd_s, snd_pos) -> Right (fct a, snd_s, snd_pos)
      Left a -> Left a
    Left a -> Left a
  a *> b = seq <$> a <*> b
  a <* b = const <$> a <*> b

instance Alternative Parser where
  empty = Parser $ \_ _ -> Left $ StackTrace []
  first <|> second =
    Parser
      ( \s pos -> case (runParser first s pos, runParser second s pos) of
          (Right (e, snd_s, snd_pos), Right _) -> Right (e, snd_s, snd_pos)
          (Right (e, snd_s, snd_pos), Left _) -> Right (e, snd_s, snd_pos)
          (Left _, Right (e, snd_s, snd_pos)) -> Right (e, snd_s, snd_pos)
          (Left a, Left _) -> Left a
      )

instance Monad Parser where
  (>>) = (*>)
  a >>= fct = Parser $ \string pos -> case runParser a string pos of
    Right (res, new_s, new_pos) -> case runParser (fct res) new_s new_pos of
      Right (new, snd_s, snd_pos) -> Right (new, snd_s, snd_pos)
      Left err -> Left err
    Left err -> Left err
  return = pure