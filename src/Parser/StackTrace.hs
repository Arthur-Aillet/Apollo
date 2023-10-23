{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module Parser.StackTrace (StackTrace (..)) where

import Parser.Range (Range (..), addNewMessage)

newtype StackTrace = StackTrace {errors :: [(String, Range)]}

instance Show StackTrace where
  show (StackTrace list) = foldr addNewMessage "Errors are: \n" list
