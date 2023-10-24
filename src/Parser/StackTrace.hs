{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module Parser.StackTrace (StackTrace (..), SourceLocation (..),
            addNewMessage, addSourceLocation, defaultLocation) where

import Parser.Range (Range (..))

newtype StackTrace = StackTrace [(String, Range, SourceLocation)]

data SourceLocation = SourceLocation
  { functionName :: String,
    fileName :: String,
    l :: Int,
    c :: Int
  }

defaultLocation :: SourceLocation
defaultLocation = SourceLocation {functionName = "", fileName = "", l = 0, c = 0}

addSourceLocation :: SourceLocation -> String -> String
addSourceLocation src str =
  str
    ++ functionName src
    ++ "' "
    ++ fileName src
    ++ ":"
    ++ show (l src)
    ++ ":"
    ++ show (c src)
    ++ "'"

addNewMessage :: (String, Range, SourceLocation) -> String -> String
addNewMessage (str, (Range start end), src) pre =
  pre
    ++ "\tError: "
    ++ str
    ++ " at "
    ++ show start
    ++ ":"
    ++ show end
    ++ ",\n(at '"
    ++ functionName src
    ++ "' "
    ++ fileName src
    ++ ":"
    ++ show (l src)
    ++ ":"
    ++ show (c src)
    ++ ")"

instance Show StackTrace where
  show (StackTrace list) = foldr addNewMessage "Errors are: \n" list
