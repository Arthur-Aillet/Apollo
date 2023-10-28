{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts Contxt
-}

import Control.Applicative ()

type Error = String

type Warning = String

data Compile a
  = Ok [Warning] a
  | Ko Error

instance Functor Compile where
  fmap fct compiler = case compiler of
    Ok warns val -> Ok warns (fct val)
    Ko err -> Ko err

instance Applicative Compile where
  pure = Ok []
  (<*>) fct compiler = case compiler of
    Ok warns1 val1 -> case fct of
      Ok warns2 fct2 -> Ok (warns1 ++ warns2) (fct2 val1)
      Ko err -> Ko err
    Ko err -> Ko err

  a *> b = seq <$> a <*> b
  a <* b = const <$> a <*> b
