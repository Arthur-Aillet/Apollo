{-
-- EPITECH PROJECT, 2023
-- Main.hs
-- File description:
-- Position type
-}

module Parser.StackTrace
  ( StackTrace (..),
    SourceLocation (..),
    addNewMessage,
    addSourceLocation,
    defaultLocation,
    modifySourceLocation,
  )
where

import Parser.Position (Position (..), defaultPosition)
import Parser.Range (Range (..))
import Ast.Display(red, yellow, resetColor)

newtype StackTrace = StackTrace [(String, Range, SourceLocation)]

data SourceLocation = SourceLocation
  { functionName :: String,
    fileName :: String,
    pos :: Position
  }

defaultLocation :: SourceLocation
defaultLocation =
  SourceLocation {functionName = "", fileName = "", pos = defaultPosition}

addSourceLocation :: String -> Position -> SourceLocation
addSourceLocation name p =
  SourceLocation
    { functionName = name,
      fileName = "",
      pos = p
    }

modifySourceLocation :: SourceLocation -> [(String, Range, SourceLocation)] -> [(String, Range, SourceLocation)]
modifySourceLocation _ stack | null stack = stack
modifySourceLocation source ((str, ran, src) : stack)
  | functionName src == "" =
      (str, ran, source) : modifySourceLocation source stack
modifySourceLocation _ stack = stack

addNewMessage :: (String, Range, SourceLocation) -> String -> String
addNewMessage ("", _, _) pre = pre
addNewMessage (str, Range start end, src) pre =
  pre
    ++ "\t in "
    ++ show (functionName src)
    ++ "("
    ++ show (pos src)
    ++ "): "
    ++ yellow ++ str ++ resetColor
    ++ " started at "
    ++ show start
    ++ " and finished at "
    ++ show end
    ++ "\n"



instance Show StackTrace where
  show (StackTrace list) = foldr addNewMessage msg list
    where
      msg | length list <= 1 = (red ++ "Error found during parsing:\n" ++ resetColor)
          | otherwise = (red ++ "Errors found during parsing:\n" ++ resetColor)

instance Eq SourceLocation where
  (SourceLocation fn1 file1 p1) == (SourceLocation fn2 file2 p2) =
    fn1 == fn2 && file1 == file2 && p1 == p2

instance Eq StackTrace where
  (StackTrace xs) == (StackTrace ys) = xs == ys