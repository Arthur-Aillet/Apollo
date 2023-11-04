{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Array Compilation
-}

module Ast.CompileArray (module Ast.CompileArray) where

import Ast.Ast
  ( Operable (..),
    Type (..),
  )
import Ast.Context (Context (..), LocalContext (..))
import Ast.Error (Compile (..), Warning, failingComp)
import Ast.Operable (compOperable)
import Ast.Utils (allEqual, listInner, zip5)
import Eval.Exec

tInList :: Type -> Type -> Int -> Maybe String
tInList val final 0
  | val == final = Nothing
  | otherwise =
      Just $
        "Array assignation: Can't assign "
          ++ show final
          ++ " to "
          ++ show val
tInList (TypeList (Just a)) final depth = tInList a final (depth - 1)
tInList list final _ =
  Just $
    "Array assignation: Can't assign "
      ++ show final
      ++ " to "
      ++ show list

compArrErr :: Bool -> [Warning] -> Operable -> Context -> LocalContext -> Compile (Insts, LocalContext)
compArrErr True w op c l =
  failingComp (compOperable op c l) w ["Indexes has to be ints"]
compArrErr False w op c l =
  failingComp (compOperable op c l) w ["Different types given in the list"]

makeFType :: Compile [Type] -> Compile [Instruction] -> Compile [(Insts, Type)] -> Compile (Insts, Type) -> Compile (Bool, Type, [Instruction], Int, (Insts, Type))
makeFType idx_types idx_insts idx_elem comp_op =
  zip5
    <$> (allEqual <$> idx_types)
    <*> (head <$> idx_types)
    <*> idx_insts
    <*> (length <$> idx_elem)
    <*> comp_op

compArrAssignation :: [Operable] -> Operable -> Index -> Type -> Context -> LocalContext -> Compile (Insts, LocalContext)
compArrAssignation idx_ops val idx wt ctx l = case final_type of
  Ko w e -> Ko w e
  Ok w (True, TypeInt, insts, len, (val_insts, ty)) -> case tInList wt ty len of
    Nothing -> Ok w (val_insts ++ insts ++ [Take len] ++ [ArrAssign idx], l)
    Just err -> Ko w [err]
  Ok w (b, _, _, _, _) -> compArrErr b w val ctx l
  where
    final_type = makeFType (map snd <$> ide) i_ins ide (compOperable val ctx l)
    i_ins = concat <$> (map fst <$> ide)
    ide = listInner $ map (\x -> compOperable x ctx l) (reverse idx_ops)
