{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST
-}

module Ast.Type (Ast(..)) where

import Atom.Atom

data Function = Function [(String, Operable)] Ast

data Definition
  =
    FuncDefinition String Function -- define a function
  | VarDefinition String Operable -- define a variable

data Structure -- layout, structure and connection of statements, having no value
  =
    Resolved -- expression resolving to no value
  | If Operable Ast Ast -- branching condition (if (x) {} {})
  | Single Ast -- single operation or operable ({x})
  | Block [Ast] [String] -- several actions ordered by variable precedence ({x;y})
  | Sequence [Ast] -- several actions ordered by precedence ({x >> y})
  deriving (Show, Eq)

data Operation -- statement involving an action, resulting in a value
  =
    Interrupt String -- Interrupt program flow
  | CallStd String [Operable] -- call a standard or builtin operation (x(y))
  | CallFunc String [Operable] -- call a function, exposes both inherent IOPipes (x(y))
  | CallSH String [Operable] -- syscall of builtin program ($x(y)), exposes both IOPipes
  | Pipe Operable Operable -- stdout mapped to stdin ({x.y}, {x <- y})
  deriving (Show, Eq)

data Operable -- statement having a value
  =
    Variable String -- Variable reffering to single known value
  | Value Atom -- Single known value
  | Operation Operation -- operation resulting in an operable value
  | IOPipe String -- named pipe, String is likely a placeholder
  deriving (Show, Eq)

data Ast
  =
    Structure Structure -- structure block ({})
  | Operable Operable -- inherent value (x)
  deriving (Show, Eq)
