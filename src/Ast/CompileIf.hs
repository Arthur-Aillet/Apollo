{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- if-then-else Compilation
-}

module Ast.CompileIf (module Ast.CompileIf) where

import Ast.Context (Compiler, Context (..), LocalContext (..))
import Ast.Error (Compile (..))
import Ast.Operable (compOperable, concatInner)
import Ast.Type
  ( Ast (..),
    Operable (..),
    Type (..),
  )
import Eval.Exec

ifOpErr :: Type -> String
ifOpErr ot = "If contain invalid type \"" ++ show ot ++ "\" instead of Bool"

compIfOp :: Operable -> Context -> LocalContext -> (Compile Insts, Int)
compIfOp op c l = case compOperable op c l of
  Ko w e -> (Ko w e, 0)
  Ok w (op_insts, TypeBool) -> (Ok w op_insts, length op_insts)
  Ok w (_, op_type) -> (Ko w [ifOpErr op_type], 0)

compIfAst :: Compiler -> Ast -> Context -> LocalContext -> (Compile Insts, Int)
compIfAst compiler ast c l = case compiler ast c l of
  Ko w e -> (Ko w e, 0)
  Ok w (insts, _) -> (Ok w insts, length insts)

compIf :: Compiler -> [(Operable, Ast)] -> Maybe Ast -> Context -> LocalContext -> (Compile Insts, Int)
compIf compiler ((op, then') : xs) e c l =
  (concatInner [op_i, j_f, ast_i, jump_n, next_i], ast_len + op_len + next_len)
  where
    j_f = Ok [] [JumpIfFalse (ast_len + plus_one)]
    plus_one = if next_len == 0 then 0 else 1
    jump_n = if next_len == 0 then Ok [] [] else Ok [] [Jump next_len]
    (next_i, next_len) = compIf compiler xs e c l
    (op_i, op_len) = compIfOp op c l
    (ast_i, ast_len) = compIfAst compiler then' c l
compIf compiler [] (Just else') c l = compIfAst compiler else' c l
compIf _ [] Nothing _ _ = (Ok [] [], 0)
