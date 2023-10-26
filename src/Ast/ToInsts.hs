{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts
-}

module Ast.ToInsts (module Ast.ToInsts) where

import Ast.Type
  ( Ast (..),
    Definition (..),
    Function (..),
    Operable (..),
    Operation (CallFunc, CallStd),
    Structure (..),
    Type (..),
    numType,
    atomType,
  )

import Ast.Context (Index(..), Context(..), LocalContext(..), createCtx, createLocalContext)

import Eval.Atom (Atom (AtomI))
import Eval.Builtins (operatorArgCount)
import Data.HashMap.Lazy (empty, (!?))
import Ast.Utils ((++++))

import Eval.Exec
import Eval.Builtins
import Debug.Trace
import Eval.Instructions

data Binary = Binary Env Func deriving (Show)

createGcd :: Definition
createGcd =
  FuncDefinition
    "gcd"
    ( Function
        [("x", TypeInt), ("y", TypeInt)]
        (Just TypeInt)
        ( AstStructure
            ( If
                (OpOperation $ CallStd Eq [OpValue (AtomI 0), OpVariable "y"])
                (AstStructure $ Return $ OpVariable "x")
                ( AstStructure $
                    Return $
                      OpOperation $
                        CallFunc
                          "gcd"
                          [OpVariable "y", OpOperation (CallStd Modulo [OpVariable "x", OpVariable "y"])]
                )
            )
        )
    )

toInsts :: [Definition] -> Either String Binary
toInsts defs = case createCtx defs (Context empty) 0 of
  Left str -> Left str
  Right ctx -> convAllFunc defs (Binary [] []) ctx

convAllFunc :: [Definition] -> Binary -> Context -> Either String Binary
convAllFunc ((FuncDefinition "main" func) : xs) (Binary env []) ctx = case convFunc func ctx of
  Left err -> Left err
  Right func -> convAllFunc xs (Binary env func) ctx
convAllFunc ((VarDefinition _ _) : _) _ _ = Left "Error: Global Variables not supported yet"
convAllFunc ((FuncDefinition _ (Function args y z)) : xs) (Binary env funcs) ctx = case convFunc (Function args y z) ctx of
  Left err -> Left err
  Right func -> convAllFunc xs (Binary (env ++ [(length args, func)]) funcs) ctx
convAllFunc [] bin _ = Right bin

convFunc :: Function -> Context -> Either String Insts
convFunc (Function args output ast) ctx = case createLocalContext args output of
  Left err -> Left err
  Right local -> convAst ast ctx local

convStruct :: Structure -> Context -> LocalContext -> Either String Insts
convStruct Resolved _ _ = Left "Err: Resolved unsupported"
convStruct (Return _) _ (LocalContext _ Nothing) = Left "Err: Return value in void function"
convStruct (Return ope) c (LocalContext a (Just fct_type)) =
  case convOperable ope c (LocalContext a (Just fct_type)) of
    Left err -> Left err
    Right (op_compiled, op_type) -> if op_type == fct_type
      then Right $ op_compiled ++ [Ret]
      else Left "Err: Return invalid type"
convStruct (If op ast_then ast_else) c l = case convOperable op c l of
  Left err -> Left err
  Right (op_compiled, op_type) -> case convAst ast_then c l of
    Left err -> Left err
    Right then_insts -> if numType op_type then
      concatInner
        [Right op_compiled,
        Right [JumpIfFalse (length then_insts)],
        Right then_insts,
        convAst ast_else c l]
      else
        Left "Err: Operator in if not numerical value"
convStruct (Single _) _ _ = Left "Err: Single unsupported"
convStruct (Block _ _) _ _ = Left "Err: Block unsupported"
convStruct (Sequence _) _ _ = Left "Err: Sequence unsupported"

convOperable :: Operable -> Context -> LocalContext -> Either String (Insts, Type)
convOperable (OpValue val) _ _ = Right ([PushD val], atomType val)
convOperable (OpVariable name) _ (LocalContext hash _) = case hash !? name of
  Nothing -> Left $ "Variable: " ++ name ++ " never defined"
  Just (index, var_type) -> Right ([PushI index], var_type)
convOperable (OpOperation op) c l = convOperation op c l
convOperable (OpIOPipe _) _ _ = Left "Err: OpIOPipe unsupported"

concatInner :: [Either a [b]] -> Either a [b]
concatInner = foldl (\a b -> (++) <$> a <*> b) (Right [])

listInner :: [Either a b] -> Either a [b]
listInner = foldl (\a b -> (++) <$> a <*> ((: []) <$> b)) (Right [])

argsHasError :: Either String [Type] -> [(String, Type)] -> Maybe String
argsHasError (Left err) _ = Just err
argsHasError (Right (given_type:xs)) ((arg_name, arg_type):ys) =
  if given_type == arg_type then
    argsHasError (Right xs) ys
  else
    Just $ "Err: " ++ arg_name ++ " invalid type"
argsHasError (Right []) (x:xs) = Just "Too few arguments"
argsHasError (Right (x:xs)) [] = Just "Too many arguments"
argsHasError (Right []) [] = Nothing

convOperation :: Operation -> Context -> LocalContext -> Either String (Insts, Type)
convOperation (CallStd builtin ops) c l =
  if length ops == operatorArgCount builtin
    then (\a -> (a, TypeInt)) <$> ((++) <$> concatInner (map (\op -> fst <$> convOperable op c l) ops) <*> Right [Op builtin])
    else Left $ "Err: Invalid number of args"
convOperation (CallFunc func ops) (Context ctx) l = case ctx !? func of
  Nothing -> Left "Err: Function name not found"
  Just (_, func_args, out) -> case argsHasError types func_args of
      Just err -> Left err
      Nothing -> (\a -> (a, TypeInt)) <$> compiled
    where
      compiled = concat <$> listInner (map (fst <$>) args_compiled)
      types = listInner $ map (snd <$>) args_compiled
      args_compiled =  map (\op -> convOperable op (Context ctx) l) ops

  -- trace ((concatMap (\op -> "\n" ++ show (convOperable op (Context ctx) l)) (ops)) ++ (concatMap (\a ->  ("\n") ++ (show $ snd a)) b) ) $ Left "Err: Not finished"
  -- Just (a, b, c) -> Left $ concat $ map (\a -> show $ snd a) b

--  <$> (\op -> (\a -> a ++ [Op builtin]) <$> convOperable op c l) ops
convOperation a _ _ = Left $ "Err: Operation unsupported" ++ show a


{-
= Interrupt String -- Interrupt program flow
\| CallStd String [Operable] -- call a standard or builtin operation (x(y))
\| CallFunc String [Operable] -- call a function, exposes both inherent IOPipes (x(y))
\| CallSH String [Operable] -- syscall of builtin program ($x(y)), exposes both IOPipes
\| Pipe Operable Operable -- stdout mapped to stdin ({x.y}, {x <- y})
-}
convAst :: Ast -> Context -> LocalContext -> Either String Insts
convAst (AstStructure struct) c l = convStruct struct c l
convAst (AstOperation op) c l = fst <$> convOperation op c l
