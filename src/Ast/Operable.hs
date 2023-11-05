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
import Ast.Type (Operable (..), Operation (CallFunc, CallStd, CallSys), Type (..), atomType, valueType)
import Ast.Utils (allEqual, concatInner, listInner, zip4)
import Data.HashMap.Lazy ((!?))
import Eval.Atom (Atom)
import Eval.Instructions (Instruction (..), Insts)
import Eval.Operator (Operator (..), OperatorDef (..), OperatorType (..), Value (..), defsOp, operate)
import Eval.Syscall (Syscall (..))

makeOPType :: Compile [Type] -> Compile [(Insts, Type)] -> Compile (Bool, Type, [Instruction], Int)
makeOPType c_types c_elem =
  zip4
    <$> (allEqual <$> c_types)
    <*> (head <$> c_types)
    <*> (concat <$> (map fst <$> c_elem))
    <*> (length <$> c_elem)

compOperable :: Operable -> Context -> LocalContext -> Compile (Insts, Type)
compOperable (OpList []) _ _ = Ok [] ([Take 0], TypeList Nothing)
compOperable (OpList array) ctx l = case final_type of
  Ko w e -> Ko w e
  Ok w (True, type', insts, len) ->
    Ok w (insts ++ [Take len], TypeList $ Just type')
  Ok w (False, _, _, _) -> Ko w ["Different types given in the list"]
  where
    final_type = makeOPType c_types c_elem
    c_types = map snd <$> c_elem
    c_elem = listInner $ map (\x -> compOperable x ctx l) (reverse array)
compOperable (OpValue val) _ _ =
  Ok [] ([PushD val], valueType (VAtom val))
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
      | otherwise -> Ok w (fop ++ [Cast ntype], ntype)
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

typeErr :: Show a => a -> Type -> Type -> String
typeErr op at wt =
  "Builtin \""
    ++ show op
    ++ "\": recieved "
    ++ show at
    ++ " when "
    ++ show wt
    ++ " was awaited"

opeValidArgs :: Show a => a -> [Compile (Insts, Type)] -> Int -> Maybe Type -> Compile Type
opeValidArgs op (Ko w err : xs) nbr type' =
  failingComp (opeValidArgs op xs (nbr - 1) type') w err
opeValidArgs _ [] 0 (Just waited_type) = Ok [] waited_type
opeValidArgs op [] nbr (Just _)
  | nbr < 0 = Ko [] ["Builtin\"" ++ show op ++ "\": Too many arguments"]
  | otherwise = Ko [] ["Builtin\"" ++ show op ++ "\": Not enough arguments"]
opeValidArgs op (Ok w _ : _) 0 (Just _) =
  Ko w ["Builtin\"" ++ show op ++ "\": Too many arguments"]
opeValidArgs op [] _ Nothing =
  Ko [] ["Builtin\"" ++ show op ++ "\": No arguments given"]
opeValidArgs op (Ok _ (_, arg_type) : xs) nbr Nothing =
  opeValidArgs op xs (nbr - 1) (Just arg_type)
opeValidArgs op (Ok w (_, at) : xs) nbr (Just wt)
  | at == wt = opeValidArgs op xs (nbr - 1) (Just wt)
  | otherwise = case opeValidArgs op xs (nbr - 1) (Just wt) of
      Ok w2 _ -> Ko (w ++ w2) [typeErr op at wt]
      Ko w2 e2 -> Ko (w ++ w2) $ typeErr op at wt : e2

compCalculus :: Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compCalculus op args count = case opeValidArgs op args count Nothing of
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

compPrinting :: Syscall -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compPrinting = compSyscallType (Just $ TypeList $ Just TypeChar) Nothing

cMsg :: [String]
cMsg = ["Can't get on empty list"]

cMsg1 :: [String]
cMsg1 = ["Get operate only on lists"]

checkGetArgs :: [Compile (Insts, Type)] -> Compile Type
checkGetArgs (x : y : ys)
  | length (x : y : ys) /= 2 = Ko [] ["Get take two arguments"]
  | otherwise = case (x, y) of
      (Ok w1 (_, TypeList (Just typ)), Ok w2 (_, TypeInt)) -> Ok (w1 ++ w2) typ
      (Ok w1 (_, TypeList Nothing), Ok w2 (_, TypeInt)) -> Ko (w1 ++ w2) cMsg
      (Ok w1 (_, _), Ok w2 (_, TypeInt)) -> Ko (w1 ++ w2) cMsg1
      (Ok w1 (_, _), Ok w2 (_, _)) -> Ko (w1 ++ w2) ["Get take a int as index"]
      (Ko w1 e1, Ok w2 _) -> Ko (w1 ++ w2) e1
      (Ok w1 _, Ko w2 e2) -> Ko (w1 ++ w2) e2
      (Ko w1 e1, Ko w2 e2) -> Ko (w1 ++ w2) (e1 ++ e2)
checkGetArgs _ = Ko [] ["Get take two arguments"]

compGet :: Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compGet op args _ = case checkGetArgs args of
  Ko warns err -> Ko warns err
  Ok w type' ->
    (\a -> (a, Just type'))
      <$> ( (++)
              <$> concatInner (map (fst <$>) (reverse args))
              <*> Ok w [Op op]
          )

compLen :: Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compLen op args count = case opeValidArgs op args count Nothing of
  Ko warns err -> Ko warns err
  Ok w type' -> case type' of
    TypeList _ ->
      (\a -> (a, Just TypeInt))
        <$> ( (++)
                <$> concatInner (map (fst <$>) (reverse args))
                <*> Ok w [Op op]
            )
    _ -> Ko w ["Concatenation take lists as input"]

compConcat :: Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compConcat op args count = case opeValidArgs op args count Nothing of
  Ko warns err -> Ko warns err
  Ok w type' -> case type' of
    TypeList _ ->
      (\a -> (a, Just type'))
        <$> ( (++)
                <$> concatInner (map (fst <$>) (reverse args))
                <*> Ok w [Op op]
            )
    _ -> Ko w ["Concatenation take lists as input"]

compOperationType :: Maybe Type -> Maybe Type -> Operator -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compOperationType in' out op args count = case opeValidArgs op args count in' of
  Ko warns err -> Ko warns err
  Ok w _ ->
    (\a -> (a, out))
      <$> ( (++)
              <$> concatInner (map (fst <$>) (reverse args))
              <*> Ok w [Op op]
          )

compSyscallType :: Maybe Type -> Maybe Type -> Syscall -> [Compile (Insts, Type)] -> Int -> Compile (Insts, Maybe Type)
compSyscallType in' out op args count = case opeValidArgs op args count in' of
  Ko warns err -> Ko warns err
  Ok w _ ->
    (\a -> (a, out))
      <$> ( (++)
              <$> concatInner (map (fst <$>) (reverse args))
              <*> Ok w [Sys op]
          )

allOfType :: Operator -> [Atom] -> Int -> Maybe Type -> Compile Type
allOfType _ [] 0 (Just waited_type) = Ok [] waited_type
allOfType op [] nbr (Just _)
  | nbr < 0 = Ko [] ["Builtin\"" ++ show op ++ "\": Too many arguments"]
  | otherwise = Ko [] ["Builtin\"" ++ show op ++ "\": Not enough arguments"]
allOfType op (_ : _) 0 (Just _) =
  Ko [] ["Builtin\"" ++ show op ++ "\": Too many arguments"]
allOfType op [] _ Nothing =
  Ko [] ["Builtin\"" ++ show op ++ "\": No arguments given"]
allOfType op (val : xs) nbr Nothing =
  allOfType op xs (nbr - 1) (Just $ atomType val)
allOfType op (val : xs) nbr (Just wt)
  | atomType val == wt = allOfType op xs (nbr - 1) (Just wt)
  | otherwise = case allOfType op xs (nbr - 1) (Just wt) of
      Ok w _ -> Ko w [typeErr op (atomType val) wt]
      Ko w e -> Ko w $ typeErr op (atomType val) wt : e

operateToCompile :: Either String Atom -> Compile Atom
operateToCompile (Left err) = Ko [] [err]
operateToCompile (Right atom) = Ok [] atom

evalCalculus :: Operator -> [Atom] -> Int -> Compile (Insts, Maybe Type)
evalCalculus op args count = case allOfType op args count Nothing of
  Ko warns err -> Ko warns err
  Ok w return_type ->
    withW w $
      (\a -> (a, Just return_type))
        <$> ((\a -> [PushD a]) <$> operateToCompile (operate op args))

evalEquality :: Operator -> [Atom] -> Int -> Compile (Insts, Maybe Type)
evalEquality op args count = case allOfType op args count Nothing of
  Ko warns err -> Ko warns err
  Ok w _ ->
    withW w $
      (\a -> (a, Just TypeBool))
        <$> ((\a -> [PushD a]) <$> operateToCompile (operate op args))

evalLogical :: Operator -> [Atom] -> Int -> Compile (Insts, Maybe Type)
evalLogical op args count = case allOfType op args count (Just TypeBool) of
  Ko warns err -> Ko warns err
  Ok w _ ->
    withW w $
      (\a -> (a, Just TypeBool))
        <$> ((\a -> [PushD a]) <$> operateToCompile (operate op args))

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

compBuiltin :: [Compile (Insts, Type)] -> Operator -> OperatorDef -> Bool -> [Operable] -> Compile (Insts, Maybe Type)
compBuiltin _ builtin (OperatorDef ac Calculus) True ops =
  evalCalculus builtin (toVa ops) ac
compBuiltin args builtin (OperatorDef ac Calculus) False _ =
  compCalculus builtin args ac
compBuiltin _ builtin (OperatorDef ac Equality) True ops =
  evalEquality builtin (toVa ops) ac
compBuiltin args builtin (OperatorDef ac Equality) False _ =
  compEquality builtin args ac
compBuiltin _ builtin (OperatorDef ac Logical) True ops =
  evalLogical builtin (toVa ops) ac
compBuiltin args builtin (OperatorDef ac Logical) False _ =
  compLogical builtin args ac
-- compBuiltin args builtin (OperatorDef ac Printing) _ _ =
  -- compPrinting builtin args ac
compBuiltin args builtin (OperatorDef ac Concatenation) _ _ =
  compConcat builtin args ac
compBuiltin args builtin (OperatorDef ac Getting) _ _ =
  compGet builtin args ac
compBuiltin args builtin (OperatorDef ac Length) _ _ =
  compLen builtin args ac

compOperation :: Operation -> Context -> LocalContext -> Compile (Insts, Maybe Type)
compOperation (CallStd builtin ops) c l =
  compBuiltin args builtin (defsOp builtin) (allValue ops) ops
  where
    args = map (\op -> compOperable op c l) ops
compOperation (CallSys builtin ops) c l =
  compPrinting builtin args 1
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
