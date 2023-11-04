{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- for Compilation
-}

module Ast.CompileFor (module Ast.CompileFor) where

import Ast.Context
import Ast.Error
import Ast.Operable (compOperable)
import Ast.Type
import Data.HashMap.Lazy (insert)
import Eval.Exec
import Debug.Trace

{--
for i in arr {

}

0) Create Counter
1) Generate Array
2) Get length
3) Counter > Length

4) Content Ast

5) Jump at (1)
--}

compForConstruct :: Insts -> Insts -> Int -> Int -> Int -> LocalContext -> Compile (Insts, LocalContext)
compForConstruct arr_insts content_insts count_pos arr_pos iter_pos l = Ok [] (final, l)
  where
    final = setup ++ fst_part ++ last_part ++ [Jump ((length fst_part + length last_part) * (-1))]
    last_part =
      [PushI count_pos, PushI arr_pos, Op Get, Assign iter_pos] -- Assign Iter
        ++ [PushI count_pos, PushD (VAtom $ AtomI 1), Op Add, Assign count_pos] -- Incr count
        ++ content_insts
    fst_part =
      [PushI arr_pos, Op Len] -- Gen Len list
        ++ [PushI count_pos, Op NEq] -- Reach End
        ++ [JumpIfFalse (length last_part + 1)]
    setup =
      arr_insts
        ++ [Store] -- Store arr
        ++ [PushI arr_pos, Op Len] -- Gen Len list
        ++ [PushD (VAtom $ AtomI 0), Op NEq] -- Reach End
        ++ [JumpIfFalse (length fst_part + length last_part + 1 + 6)]
        ++ [PushD (VAtom $ AtomI 0), Store] -- Store count
        ++ [PushI count_pos, PushI arr_pos, Op Get, Store] -- Store Iter

forWTypeErr :: Type -> [Error]
forWTypeErr wrong_type = ["In for, " ++ show wrong_type ++ "ins'nt operable"]

compForAst :: Type -> Insts -> Compiler -> String -> Ast -> Context -> LocalContext -> Compile (Insts, LocalContext)
compForAst inner arr_insts cAst iter_name ast ctx (LocalContext hmap r) =
  case cAst ast ctx (LocalContext hmap_iter r) of
    Ko w e -> Ko w e
    Ok w (in_insts, l) -> withW w $ compForConstruct arr_insts in_insts c_pos arr_pos iter_pos l
  where
    hmap_iter = insert iter_name (iter_pos, inner, True) hmap_with_counter
    iter_pos = firstValidIndex hmap_with_counter
    hmap_with_counter =
      insert (iter_name ++ "@ForCounter") (c_pos, TypeInt, True) hmap_with_arr
    c_pos = firstValidIndex hmap_with_arr
    hmap_with_arr =
      insert (iter_name ++ "@ForArr") (arr_pos, TypeList $ Just inner, True) hmap
    arr_pos = firstValidIndex hmap

compFor :: Compiler -> String -> Operable -> Ast -> Context -> LocalContext -> Compile (Insts, LocalContext)
compFor cAst iter_name arr ast ctx (LocalContext hmap r) =
  case compOperable arr ctx (LocalContext hmap r) of
    Ko w e -> failingComp (cAst ast ctx (LocalContext hmap r)) w e
    Ok w (_, TypeList Nothing) ->
      failingComp (cAst ast ctx (LocalContext hmap r)) w [""]
    Ok w (arr_insts, TypeList (Just inner)) ->
      withW w $
        compForAst inner arr_insts cAst iter_name ast ctx (LocalContext hmap r)
    Ok w (_, wtype) ->
      failingComp (cAst ast ctx (LocalContext hmap r)) w (forWTypeErr wtype)
