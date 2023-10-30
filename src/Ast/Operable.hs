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
import Ast.Error (Compile (..), withW)
import Ast.Type (Operable (..), Operation (CallFunc, CallStd), Type (TypeBool), atomType)
import Ast.Utils (concatInner, listInner)
import Data.HashMap.Lazy ((!?))
import Eval.Instructions (Instruction (..), Insts)
import Eval.Operator (Operator, OperatorDef (..), OperatorType (..), defsOp)

compOperable :: Operable -> Context -> LocalContext -> Compile (Insts, Type)
compOperable (OpValue val) _ _ = Ok [] ([PushD val], atomType val)
compOperable (OpVariable name) _ (LocalContext hash _) = case hash !? name of
  Nothing -> Ko [] ["Variable \"" ++ name ++ "\" never declared"]
  Just (_, _, False) -> Ko [] ["Variable \"" ++ name ++ "\" never defined"]
  Just (index, var_type, True) -> Ok [] ([PushI index], var_type)
compOperable (OpOperation op) c l = case compOperation op c l of
  Ko warns err -> Ko warns err
  Ok warns (_, Nothing) -> Ko warns ["Op has no return type"]
  Ok warns (a, Just b) -> Ok warns (a, b)
compOperable (OpIOPipe _) _ _ = Ko [] ["OpIOPipe unsupported"]
compOperable (OpCast op ntype) c l =
  case compOperable op c l of
    Ko w e -> Ko w e
    Ok w (fop, ftype)
      | ntype == ftype -> withW [warn] $ Ok w (fop, ntype)
      | otherwise -> Ok w (fop, ntype)
      where
        warn = "Cast from " ++ show ntype ++ " to " ++ show ntype

argsHasError :: Compile [Type] -> [(String, Type)] -> Compile ()
argsHasError (Ko w err) _ = Ko w err
argsHasError (Ok w (given_type : xs)) ((arg_name, arg_type) : ys)
  | given_type == arg_type = argsHasError (Ok w xs) ys
  | otherwise = Ko w [arg_name ++ " has invalid type"]
argsHasError (Ok w []) (_ : _) = Ko w ["Too few arguments"]
argsHasError (Ok w (_ : _)) [] = Ko w ["Too many arguments"]
argsHasError (Ok w []) [] = Ok w ()

opeValidArgs :: [Compile (Insts, Type)] -> Int -> Maybe Type -> Compile Type
opeValidArgs (Ko w err : _) _ _ = Ko w err
opeValidArgs [] 0 (Just waited_type) = Ok [] waited_type
opeValidArgs [] _ (Just _) = Ko [] ["Builtin: Not enough arguments"]
opeValidArgs (Ok w _ : _) 0 (Just _) = Ko w ["Builtin: Too many arguments"]
opeValidArgs [] _ Nothing = Ko [] ["Builtin: No arguments given"]
opeValidArgs (Ok _ (_, arg_type) : xs) nbr Nothing =
  opeValidArgs xs (nbr - 1) (Just arg_type)
opeValidArgs (Ok w (_, arg_type) : xs) nbr (Just waited_type)
  | arg_type == waited_type = opeValidArgs xs (nbr - 1) (Just waited_type)
  | otherwise =
      Ko
        w
        [ "Builtin: recieved "
            ++ show arg_type
            ++ " when "
            ++ show waited_type
            ++ " was awaited"
        ]

compCalculus :: Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compCalculus op args count = case opeValidArgs args count Nothing of
  Ko warns err -> Ko warns err
  Ok w return_type ->
    (\a -> (a, Just return_type))
      <$> ( (++)
              <$> concatInner (map (fst <$>) (reverse args))
              <*> Ok w [Op op]
          )

compEquality :: Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compEquality op args count = case opeValidArgs args count Nothing of
  Ko warns err -> Ko warns err
  Ok w _ ->
    (\a -> (a, Just TypeBool))
      <$> ( (++)
              <$> concatInner (map (fst <$>) (reverse args))
              <*> Ok w [Op op]
          )

compLogical :: Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compLogical op args count = case opeValidArgs args count (Just TypeBool) of
  Ko warns err -> Ko warns err
  Ok w _ ->
    (\a -> (a, Just TypeBool))
      <$> ( (++)
              <$> concatInner (map (fst <$>) (reverse args))
              <*> Ok w [Op op]
          )

compOperation :: Operation -> Context -> LocalContext -> Compile (Insts, Maybe Type)
compOperation (CallStd builtin ops) c l = case defsOp builtin of
  (OperatorDef argCount Calculus) -> compCalculus builtin args argCount
  (OperatorDef argCount Equality) -> compEquality builtin args argCount
  (OperatorDef argCount Logical) -> compLogical builtin args argCount
  where
    args = map (\op -> compOperable op c l) ops
compOperation (CallFunc func ops) (Context c) l = case c !? func of
  Nothing -> Ko [] ["Function name not found"]
  Just (nb, func_args, out) -> case argsHasError types func_args of
    Ko w err -> Ko w err
    Ok w _ -> (\a -> (a, out)) <$> ((++) <$> fca <*> Ok w [CallD nb])
    where
      fca = concat <$> listInner (map (fst <$>) args_compiled)
      types = listInner $ map (snd <$>) args_compiled
      args_compiled = map (\op -> compOperable op (Context c) l) (reverse ops)
compOperation a _ _ = Ko [] ["Operation unsupported" ++ show a]
