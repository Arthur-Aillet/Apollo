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
import Ast.Error (Compile (..), failingComp, withW)
import Ast.Type (Operable (..), Operation (CallFunc, CallStd), Type (TypeBool, TypeChar, TypeList), atomType, valueType)
import Ast.Utils (concatInner, listInner)
import Data.HashMap.Lazy ((!?))
import Eval.Atom (Atom)
import Eval.Instructions (Instruction (..), Insts)
import Eval.Operator (Operator, OperatorDef (..), OperatorType (..), Value (..), defsOp, operate)

{--
[3, 4]
++ (concat)
[3, 2]

[3, 4]
++ (append)
3

4
++ (push)
[3, 5]
--}

-- compList :: Compile [Insts]

compOperable :: Operable -> Context -> LocalContext -> Compile (Insts, Type)
compOperable (OpList array) ctx l = case final_type of
  Ko w e -> Ko w e
  Ok w (True, type', insts, len) -> Ok w (insts ++ [Take len], TypeList $ Just type')
  Ok w (False, _, _, _) -> Ko w ["Different types given in the list"]
  where
    final_type = (\a b c d -> (b, head a, c, d)) <$> c_types <*> all_type <*> c_insts <*> (length <$> c_elem)
    all_type = (\x -> and $ zipWith (==) x (tail x)) <$> c_types
    c_types = map snd <$> c_elem
    c_insts = concat <$> (map fst <$> c_elem)
    c_elem = listInner $ map (\x -> compOperable x ctx l) (reverse array)
compOperable (OpValue val) _ _ = Ok [] ([PushD (VAtom val)], valueType (VAtom val))
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

typeErr :: Type -> Type -> String
typeErr at wt =
  "Builtin: recieved "
    ++ show at
    ++ " when "
    ++ show wt
    ++ " was awaited"

opeValidArgs :: [Compile (Insts, Type)] -> Int -> Maybe Type -> Compile Type
opeValidArgs (Ko w err : xs) nbr type' =
  failingComp (opeValidArgs xs (nbr - 1) type') w err
opeValidArgs [] 0 (Just waited_type) = Ok [] waited_type
opeValidArgs [] nbr (Just _)
  | nbr < 0 = Ko [] ["Builtin: Too many arguments"]
  | otherwise = Ko [] ["Builtin: Not enough arguments"]
opeValidArgs (Ok w _ : _) 0 (Just _) = Ko w ["Builtin: Too many arguments"]
opeValidArgs [] _ Nothing = Ko [] ["Builtin: No arguments given"]
opeValidArgs (Ok _ (_, arg_type) : xs) nbr Nothing =
  opeValidArgs xs (nbr - 1) (Just arg_type)
opeValidArgs (Ok w (_, at) : xs) nbr (Just wt)
  | at == wt = opeValidArgs xs (nbr - 1) (Just wt)
  | otherwise = case opeValidArgs xs (nbr - 1) (Just wt) of
      Ok w2 _ -> Ko (w ++ w2) [typeErr at wt]
      Ko w2 e2 -> Ko (w ++ w2) $ typeErr at wt : e2

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
compEquality = compOperationType Nothing (Just TypeBool)

compLogical :: Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compLogical = compOperationType (Just TypeBool) (Just TypeBool)

compPrinting :: Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compPrinting = compOperationType (Just $ TypeList $ Just TypeChar) Nothing

compOperationType :: Maybe Type -> Maybe Type -> Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compOperationType in' out op args count = case opeValidArgs args count in' of
  Ko warns err -> Ko warns err
  Ok w _ ->
    (\a -> (a, out))
      <$> ( (++)
              <$> concatInner (map (fst <$>) (reverse args))
              <*> Ok w [Op op]
          )

allOfType :: [Atom] -> Int -> Maybe Type -> Compile Type
allOfType [] 0 (Just waited_type) = Ok [] waited_type
allOfType [] nbr (Just _)
  | nbr < 0 = Ko [] ["Builtin: Too many arguments"]
  | otherwise = Ko [] ["Builtin: Not enough arguments"]
allOfType (_ : _) 0 (Just _) = Ko [] ["Builtin: Too many arguments"]
allOfType [] _ Nothing = Ko [] ["Builtin: No arguments given"]
allOfType (val : xs) nbr Nothing =
  allOfType xs (nbr - 1) (Just $ atomType val)
allOfType (val : xs) nbr (Just wt)
  | atomType val == wt = allOfType xs (nbr - 1) (Just wt)
  | otherwise = case allOfType xs (nbr - 1) (Just wt) of
      Ok w _ -> Ko w [typeErr (atomType val) wt]
      Ko w e -> Ko w $ typeErr (atomType val) wt : e

operateToCompile :: Either String Atom -> Compile Atom
operateToCompile (Left err) = Ko [] [err]
operateToCompile (Right atom) = Ok [] atom

evalCalculus :: Operator -> [Atom] -> Int -> Compile (Insts, Maybe Type)
evalCalculus op args count = case allOfType args count Nothing of
  Ko warns err -> Ko warns err
  Ok w return_type ->
    withW w $
      (\a -> (a, Just return_type))
        <$> ((\a -> [PushD $ VAtom a]) <$> operateToCompile (operate op args))

evalEquality :: Operator -> [Atom] -> Int -> Compile (Insts, Maybe Type)
evalEquality op args count = case allOfType args count Nothing of
  Ko warns err -> Ko warns err
  Ok w _ ->
    withW w $
      (\a -> (a, Just TypeBool))
        <$> ((\a -> [PushD $ VAtom a]) <$> operateToCompile (operate op args))

evalLogical :: Operator -> [Atom] -> Int -> Compile (Insts, Maybe Type)
evalLogical op args count = case allOfType args count (Just TypeBool) of
  Ko warns err -> Ko warns err
  Ok w _ ->
    withW w $
      (\a -> (a, Just TypeBool))
        <$> ((\a -> [PushD $ VAtom a]) <$> operateToCompile (operate op args))

allValue :: [Operable] -> Bool
allValue =
  foldl
    ( \bool op -> case (op, bool) of
        (OpValue _, True) -> True
        _ -> False
    )
    True

toVa :: [Operable] -> [Atom]
toVa =
  foldl
    ( \arr op -> case op of
        (OpValue atom) -> arr ++ [atom]
        _ -> []
    )
    []

-- operate :: Operator -> ([Atom] -> Either String Atom)

compOperation :: Operation -> Context -> LocalContext -> Compile (Insts, Maybe Type)
compOperation (CallStd builtin ops) c l = case (defsOp builtin, allValue ops) of
  (OperatorDef ac Calculus, True) -> evalCalculus builtin (toVa ops) ac
  (OperatorDef ac Calculus, False) -> compCalculus builtin args ac
  (OperatorDef ac Equality, True) -> evalEquality builtin (toVa ops) ac
  (OperatorDef ac Equality, False) -> compEquality builtin args ac
  (OperatorDef ac Logical, True) -> evalLogical builtin (toVa ops) ac
  (OperatorDef ac Logical, False) -> compLogical builtin args ac
  (OperatorDef ac Printing, _) -> compPrinting builtin args ac
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
