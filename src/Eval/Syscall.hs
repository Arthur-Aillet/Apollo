{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Instructions
-}

module Eval.Syscall (Syscall (..), execSys) where

import Eval.Operator
    ( Value(..), Stack, Operator(..), Stack, Value(..) )
import Eval.Atom (Atom (..))

data Syscall
  = Print
  deriving (Show, Eq)

execSys :: Stack -> Syscall -> IO (Either String Stack)
execSys (x : xs) Print = case x of
  (VList []) -> return (Right xs)
  (VList (VAtom (AtomC c _) : chars)) ->
    putStr [c] >> execSys (VList chars : xs) Print
  _ -> return (Left "Print with non string")
execSys [] Print = return $ Left "Print with empty stack"
