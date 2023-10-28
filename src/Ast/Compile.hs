{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts
-}

module Ast.Compile (module Ast.Compile) where

import Ast.Context (Context (..), CurrentReturnType, LocalContext (..), Variables, createCtx, createLocalContext, firstValidIndex)
import Ast.Error (Compile (..), withW)
import Ast.Operable (compOperable, compOperation, concatInner)
import Ast.Type
  ( Ast (..),
    Definition (..),
    Function (..),
    Operable (..),
    Structure (..),
    Type (..),
    numType,
  )
import Data.HashMap.Lazy (adjust, empty, insert, member, (!?))
import Eval.Exec

data Binary = Binary Env Func deriving (Show)

compile :: [Definition] -> Compile Binary
compile defs = case createCtx defs (Context empty) 0 of
  Ko warns err -> Ko warns err
  Ok warns ctx -> case compAllFunc defs (Binary [] []) ctx of
    Ko warns2 err -> withW warns $ Ko warns2 err
    Ok warns2 (Binary _ []) -> withW warns $ Ko warns2 "no main function found"
    other -> other

compAllFunc :: [Definition] -> Binary -> Context -> Compile Binary
compAllFunc ((FuncDefinition "main" func) : xs) (Binary env []) ctx =
  case compFunc func ctx of
    Ko warns err -> Ko warns err
    Ok w function -> withW w $ compAllFunc xs (Binary env function) ctx
compAllFunc ((FuncDefinition _ (Function args y z)) : xs) (Binary env funcs) c =
  case compFunc (Function args y z) c of
    Ko warns err -> Ko warns err
    Ok w f ->
      withW w $ compAllFunc xs (Binary (env ++ [(length args, f)]) funcs) c
compAllFunc [] bin _ = Ok [] bin

compFunc :: Function -> Context -> Compile Insts
compFunc (Function args output ast) ctx = case createLocalContext args output of
  Ko warns err -> Ko warns err
  Ok w local -> withW w $ fst <$> compAst ast ctx local

compAst :: Ast -> Context -> LocalContext -> Compile (Insts, LocalContext)
compAst (AstStructure struct) c l = compStruct struct c l
compAst (AstOperation op) c l =
  (\a -> (a, l)) <$> (fst <$> compOperation op c l)

compIf :: Compile Insts -> Compile Insts -> Type -> Compile Insts -> Int -> Compile Insts
compIf op_compiled else_comp op_type then_insts len
  | numType op_type =
      concatInner
        [ op_compiled,
          Ok [] [JumpIfFalse len],
          then_insts,
          else_comp
        ]
  | otherwise = Ko [] "Operator in if not numerical value"

packCompIf :: Compile Insts -> Compile Insts -> Ast -> Context -> LocalContext -> Int -> Compile (Insts, LocalContext)
packCompIf op_compiled then_insts ast_else c l len =
  (\a -> (a, l))
    <$> compIf
      op_compiled
      (fst <$> compAst ast_else c l)
      TypeBool
      then_insts
      len

compVarDefinition :: Operable -> Variables -> Maybe Type -> Type -> String -> Context -> Compile (Insts, LocalContext)
compVarDefinition op hmap r vtype name c =
  case compOperable op c (LocalContext hmap r) of
    Ko warns err -> Ko warns err
    Ok w (insts, op_type)
      | op_type == vtype -> Ok w (insts ++ [Store], new_local)
      | otherwise -> Ko w "Variable recieved invalid type"
  where
    new_local = LocalContext new_hmap r
    new_hmap = insert name (firstValidIndex hmap, vtype, True) hmap

setTrue :: (Index, Type, Bool) -> (Index, Type, Bool)
setTrue (a, b, _) = (a, b, True)

compFirstAssign :: Operable -> Context -> Variables -> CurrentReturnType -> Type -> String -> Compile (Insts, LocalContext)
compFirstAssign op c hmap r wtype name =
  case compOperable op c (LocalContext hmap r) of
    Ko warns err -> Ko warns err
    Ok w (insts, rtype)
      | wtype == rtype ->
          Ok w (insts ++ [Store], LocalContext (adjust setTrue name hmap) r)
      | otherwise -> Ko w $ "Variable " ++ name ++ " type redefined"

compStruct :: Structure -> Context -> LocalContext -> Compile (Insts, LocalContext)
compStruct Resolved _ _ = Ko [] "Resolved unsupported"
compStruct (Return _) _ (LocalContext _ Nothing) =
  Ko [] "Return value in void function"
compStruct (Return ope) c (LocalContext a (Just fct_type)) =
  case compOperable ope c (LocalContext a (Just fct_type)) of
    Ko warns err -> Ko warns err
    Ok w (op_compiled, op_type)
      | op_type == fct_type ->
          Ok w (op_compiled ++ [Ret], LocalContext a (Just fct_type))
      | otherwise -> Ko w "Return invalid type"
compStruct (If op ast_then a_else) c l = case compOperable op c l of
  Ko warns err -> Ko warns err
  Ok w (op_comp, TypeBool) -> case compAst ast_then c l of
    Ko warns err -> Ko warns err
    Ok w1 t_i ->
      packCompIf (Ok w op_comp) (Ok w1 (fst t_i)) a_else c l (length $ fst t_i)
  Ok w (_, op_type) ->
    Ko w $ "If wait boolean and not " ++ show op_type
compStruct (Single _) _ _ = Ko [] "Single unsupported"
compStruct (Block _ _) _ _ = Ko [] "Block unsupported"
compStruct (Sequence (x : xs)) c l = case compAst x c l of
  Ko warns err -> Ko warns err
  Ok w (insts, new_local) ->
    (\(a_i, _) (b_i, f_l) -> (a_i ++ b_i, f_l))
      <$> Ok w (insts, new_local)
      <*> compStruct (Sequence xs) c new_local
compStruct (Sequence []) _ l = Ok [] ([], l)
compStruct (VarDefinition name vtype content) c (LocalContext vs r)
  | name `member` vs = Ko [] "Variable with name already exist"
  | otherwise = case content of
      Nothing ->
        Ok
          []
          ( [],
            LocalContext (insert name (firstValidIndex vs, vtype, False) vs) r
          )
      Just op -> compVarDefinition op vs r vtype name c
compStruct (VarAssignation name op) c (LocalContext hmap r) =
  case hmap !? name of
    Nothing -> Ko [] $ "Variable " ++ name ++ "undefined"
    Just (idx, wtype, True) -> case compOperable op c (LocalContext hmap r) of
      Ko warns err -> Ko warns err
      Ok w (insts, rtype)
        | wtype == rtype -> Ok w (insts ++ [Assign idx], LocalContext hmap r)
        | otherwise -> Ko [] $ "Variable " ++ name ++ " type redefined"
    Just (_, wtype, False) -> compFirstAssign op c hmap r wtype name

{--
  case compOperable op c (LocalContext hmap r) of
    Ko warns err -> Ko warns err
    Ok w (insts, op_type) ->
      if op_type == vtype
        then Ok w (insts ++ [Store], new_local)
        else Ko "Variable recieved invalid type"
  where
    new_local = LocalContext new_hmap r
    new_hmap = insert name (firstValidIndex hmap, vtype, True) hmap
--}
