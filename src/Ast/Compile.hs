{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts
-}

module Ast.Compile (module Ast.Compile) where

import Ast.Context (Context (..), LocalContext (..), Variables, createCtx, createLocalContext, firstValidIndex, CurrentReturnType)
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

compile :: [Definition] -> Either String Binary
compile defs = case createCtx defs (Context empty) 0 of
  Left str -> Left str
  Right ctx -> case compAllFunc defs (Binary [] []) ctx of
    Left err -> Left err
    Right (Binary _ []) -> Left "Err: no main function found"
    other -> other

compAllFunc :: [Definition] -> Binary -> Context -> Either String Binary
compAllFunc ((FuncDefinition "main" func) : xs) (Binary env []) ctx =
  case compFunc func ctx of
    Left err -> Left err
    Right function -> compAllFunc xs (Binary env function) ctx
compAllFunc ((FuncDefinition _ (Function args y z)) : xs) (Binary env funcs) c =
  case compFunc (Function args y z) c of
    Left err -> Left err
    Right f -> compAllFunc xs (Binary (env ++ [(length args, f)]) funcs) c
compAllFunc [] bin _ = Right bin

compFunc :: Function -> Context -> Either String Insts
compFunc (Function args output ast) ctx = case createLocalContext args output of
  Left err -> Left err
  Right local -> fst <$> compAst ast ctx local

compAst :: Ast -> Context -> LocalContext -> Either String (Insts, LocalContext)
compAst (AstStructure struct) c l = compStruct struct c l
compAst (AstOperation op) c l =
  (\a -> (a, l)) <$> (fst <$> compOperation op c l)

compIf :: Insts -> Either String Insts -> Type -> Insts -> Either String Insts
compIf op_compiled else_comp op_type then_insts
  | numType op_type =
      concatInner
        [ Right op_compiled,
          Right [JumpIfFalse (length then_insts)],
          Right then_insts,
          else_comp
        ]
  | otherwise = Left "Err: Operator in if not numerical value"

packCompIf :: Insts -> Ast -> Context -> LocalContext -> (Insts, LocalContext) -> Either String (Insts, LocalContext)
packCompIf op_compiled ast_else c l then_insts =
  (\a -> (a, l))
    <$> compIf
      op_compiled
      (fst <$> compAst ast_else c l)
      TypeBool
      (fst then_insts)

compVarDefinition :: Operable -> Variables -> Maybe Type -> Type -> String -> Context -> Either String (Insts, LocalContext)
compVarDefinition op hmap r vtype name c =
  case compOperable op c (LocalContext hmap r) of
    Left err -> Left err
    Right (insts, op_type)
      | op_type == vtype -> Right (insts ++ [Store], new_local)
      | otherwise -> Left "Err: Variable recieved invalid type"
  where
    new_local = LocalContext new_hmap r
    new_hmap = insert name (firstValidIndex hmap, vtype, True) hmap

compFirstAssign :: Operable -> Context -> Variables -> CurrentReturnType -> Type -> String -> Either String (Insts, LocalContext)
compFirstAssign op c hmap r wtype name =
  case compOperable op c (LocalContext hmap r) of
    Left err -> Left err
    Right (insts, rtype)
      | wtype == rtype ->
          Right
            ( insts ++ [Store],
              LocalContext (adjust (\(a, b, _) -> (a, b, True)) name hmap) r
            )
      | otherwise -> Left $ "Err: Variable " ++ name ++ " type redefined"

compStruct :: Structure -> Context -> LocalContext -> Either String (Insts, LocalContext)
compStruct Resolved _ _ = Left "Err: Resolved unsupported"
compStruct (Return _) _ (LocalContext _ Nothing) =
  Left "Err: Return value in void function"
compStruct (Return ope) c (LocalContext a (Just fct_type)) =
  case compOperable ope c (LocalContext a (Just fct_type)) of
    Left err -> Left err
    Right (op_compiled, op_type)
      | op_type == fct_type ->
          Right (op_compiled ++ [Ret], LocalContext a (Just fct_type))
      | otherwise -> Left "Err: Return invalid type"
compStruct (If op ast_then ast_else) c l = case compOperable op c l of
  Left err -> Left err
  Right (op_compiled, TypeBool) -> case compAst ast_then c l of
    Left err -> Left err
    Right then_insts -> packCompIf op_compiled ast_else c l then_insts
  Right (_, op_type) ->
    Left $ "Err: If wait boolean and not " ++ show op_type
compStruct (Single _) _ _ = Left "Err: Single unsupported"
compStruct (Block _ _) _ _ = Left "Err: Block unsupported"
compStruct (Sequence (x : xs)) c l = case compAst x c l of
  Left err -> Left err
  Right (insts, new_local) ->
    (\(a_i, _) (b_i, f_l) -> (a_i ++ b_i, f_l))
      <$> Right (insts, new_local)
      <*> compStruct (Sequence xs) c new_local
compStruct (Sequence []) _ l = Right ([], l)
compStruct (VarDefinition name vtype content) c (LocalContext vs r)
  | name `member` vs = Left "Err: Variable with name already exist"
  | otherwise = case content of
      Nothing ->
        Right
          ( [],
            LocalContext (insert name (firstValidIndex vs, vtype, False) vs) r
          )
      Just op -> compVarDefinition op vs r vtype name c
compStruct (VarAssignation name op) c (LocalContext hmap r) =
  case hmap !? name of
    Nothing -> Left $ "Err: Variable " ++ name ++ "undefined"
    Just (idx, wtype, True) -> case compOperable op c (LocalContext hmap r) of
      Left err -> Left err
      Right (insts, rtype)
        | wtype == rtype -> Right (insts ++ [Assign idx], LocalContext hmap r)
        | otherwise -> Left $ "Err: Variable " ++ name ++ " type redefined"
    Just (_, wtype, False) -> compFirstAssign op c hmap r wtype name

{--
  case compOperable op c (LocalContext hmap r) of
    Left err -> Left err
    Right (insts, op_type) ->
      if op_type == vtype
        then Right (insts ++ [Store], new_local)
        else Left "Err: Variable recieved invalid type"
  where
    new_local = LocalContext new_hmap r
    new_hmap = insert name (firstValidIndex hmap, vtype, True) hmap
--}
