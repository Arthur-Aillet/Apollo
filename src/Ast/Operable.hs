{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts Operable
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Ast.Operable (concatInner, compOperable, compOperation) where

import Ast.Context (Context (Context), LocalContext (..))
import Ast.Type (Operable (..), Operation (CallFunc, CallStd), Type (TypeInt), atomType)
import Ast.Utils (concatInner, listInner)
import Data.HashMap.Lazy ((!?))
import Eval.Operator (operatorArgCount)
import Eval.Instructions (Instruction (..), Insts)

compOperable :: Operable -> Context -> LocalContext -> Either String (Insts, Type)
compOperable (OpValue val) _ _ = Right ([PushD val], atomType val)
compOperable (OpVariable name) _ (LocalContext hash _) = case hash !? name of
  Nothing -> Left $ "Variable: " ++ name ++ " never defined"
  Just (index, var_type) -> Right ([PushI index], var_type)
compOperable (OpOperation op) c l = case compOperation op c l of
  Left err -> Left err
  Right (_, Nothing) -> Left "Err: op has no return type"
  Right (a, Just b) -> Right (a, b)
compOperable (OpIOPipe _) _ _ = Left "Err: OpIOPipe unsupported"

argsHasError :: Either String [Type] -> [(String, Type)] -> Maybe String
argsHasError (Left err) _ = Just err
argsHasError (Right (given_type : xs)) ((arg_name, arg_type) : ys) =
  if given_type == arg_type
    then argsHasError (Right xs) ys
    else Just $ "Err: " ++ arg_name ++ " invalid type"
argsHasError (Right []) (_ : _) = Just "Too few arguments"
argsHasError (Right (_ : _)) [] = Just "Too many arguments"
argsHasError (Right []) [] = Nothing

compOperation :: Operation -> Context -> LocalContext -> Either String (Insts, Maybe Type)
compOperation (CallStd builtin ops) c l =
  if length ops == operatorArgCount builtin
    then
      (\a -> (a, Just TypeInt))
        <$> ( (++)
                <$> concatInner (map (\op -> fst <$> compOperable op c l) ops)
                <*> Right [Op builtin]
            )
    else Left "Err: Invalid number of args"
compOperation (CallFunc func ops) (Context ctx) l = case ctx !? func of
  Nothing -> Left "Err: Function name not found"
  Just (id, func_args, out) -> case argsHasError types func_args of
    Just err -> Left err
    Nothing -> (\a -> (a, out)) <$> ((++) <$> f_comp_args <*> Right [CallD id])
    where
      f_comp_args = concat <$> listInner (map (fst <$>) args_compiled)
      types = listInner $ map (snd <$>) args_compiled
      args_compiled = map (\op -> compOperable op (Context ctx) l) ops
compOperation a _ _ = Left $ "Err: Operation unsupported" ++ show a
