{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts Contxt
-}

module Ast.Error (Compile (..), withW) where

import Control.Applicative ()

type Error = String

type Warning = String

data Compile a
  = Ok [Warning] a
  | Ko [Warning] Error

withW :: [Warning] -> Compile a -> Compile a
withW ws (Ok w r) = Ok (ws ++ w) r
withW ws (Ko w e) = Ko (ws ++ w) e

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
    Ko warns err -> Ko warns err

  a *> b = seq <$> a <*> b
  a <* b = const <$> a <*> b
