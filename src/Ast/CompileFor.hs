{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- for Compilation
-}

module Ast.CompileFor (module Ast.CompileFor) where

import Ast.Context hiding (Index)
import Ast.Error
import Ast.Operable (compOperable)
import Ast.Type
import Data.HashMap.Lazy (insert)
import Eval.Exec

compForFstPart :: Index -> Index -> Insts -> Insts
compForFstPart arr_pos count_pos last_part = [PushI arr_pos, Op Len] -- Gen Len list
        ++ [PushI count_pos, Op NEq] -- End Reached?
        ++ [JumpIfFalse (length last_part + 1)] -- Jump

compForToStart :: Insts -> Insts -> Insts
compForToStart fst_part last_part =
  [Jump ((length fst_part + length last_part) * (-1))]

compForSetup :: Insts -> Index -> Insts -> Insts -> Index -> Insts
compForSetup arr_insts arr_pos fst_part last_part count_pos = arr_insts
        ++ [Store] -- Store arr
        ++ [PushI arr_pos, Op Len] -- Gen Len list
        ++ [PushD (AtomI 0), Op NEq] -- Reach End
        ++ [JumpIfFalse (length fst_part + length last_part + 1 + 6)]
        ++ [PushD (AtomI 0), Store] -- Store count
        ++ [PushI count_pos, PushI arr_pos, Op Get, Store] -- Store Iter

compForInsts :: Insts -> Insts -> Index -> Index -> Index -> LocalContext -> (Insts, LocalContext)
compForInsts arr_insts content_insts count_pos arr_pos iter_pos l = (final, l)
  where
    final = setup ++ fst_part ++ last_part ++ compForToStart fst_part last_part
    last_part =
      [PushI count_pos, PushI arr_pos, Op Get, Assign iter_pos] -- Assign Iter
        ++ [PushI count_pos, PushD (AtomI 1), Op Add, Assign count_pos] -- Incr count
        ++ content_insts
    fst_part = compForFstPart arr_pos count_pos last_part
    setup = compForSetup arr_insts arr_pos fst_part last_part count_pos

forWTypeErr :: Type -> [Error]
forWTypeErr wrong_type = ["In for, " ++ show wrong_type ++ "ins'nt operable"]

compForAst :: Type -> Insts -> Compiler -> String -> Ast -> Context -> LocalContext -> Compile (Insts, LocalContext)
compForAst iTyp a_insts cAst iter ast ctx (LocalContext hmap r) =
  (\(insts, l) -> compForInsts a_insts insts c_pos a_pos i_pos l)
    <$> cAst ast ctx (LocalContext hmap_iter r)
  where
    hmap_iter = insert iter (i_pos, iTyp, True) hmapC
    i_pos = firstValidIndex hmapC
    hmapC = insert (iter ++ "@ForCounter") (c_pos, TypeInt, True) hmapA
    c_pos = firstValidIndex hmapA
    hmapA = insert (iter ++ "@ForArr") (a_pos, TypeList $ Just iTyp, True) hmap
    a_pos = firstValidIndex hmap

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
