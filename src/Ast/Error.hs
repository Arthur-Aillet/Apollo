{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- AST To Insts Contxt
-}

module Ast.Error (Compile (..), withW, Warning, Error, failingComp) where

import Control.Applicative ()

type Error = String

type Warning = String

data Compile a
  = Ok [Warning] a
  | Ko [Warning] [Error]

withW :: [Warning] -> Compile a -> Compile a
withW ws (Ok w r) = Ok (ws ++ w) r
withW ws (Ko w e) = Ko (ws ++ w) e

failingComp :: Compile a -> [Warning] -> [Error] -> Compile c
failingComp a warns1 err1 = case a of
  Ok warns2 _ -> Ko (warns1 ++ warns2) err1
  Ko warns2 err2 -> Ko (warns1 ++ warns2) $ err1 ++ err2

instance Functor Compile where
  fmap fct compiler = case compiler of
    Ok warns val -> Ok warns (fct val)
    Ko warns err -> Ko warns err

instance Applicative Compile where
  pure = Ok []
  (<*>) fct compiler = case compiler of
    Ok warns1 val1 -> case fct of
      Ok warns2 fct2 -> Ok (warns1 ++ warns2) (fct2 val1)
      Ko warns err -> Ko warns err
    Ko warns1 err1 -> case fct of
      Ok warns2 _ -> Ko (warns1 ++ warns2) err1
      Ko warns2 err2 -> Ko (warns1 ++ warns2) (err1 ++ err2)

  a *> b = seq <$> a <*> b
  a <* b = const <$> a <*> b
