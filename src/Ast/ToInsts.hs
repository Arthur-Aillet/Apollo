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
import Data.HashMap.Lazy (empty, (!?))
import Ast.Utils ((++++))

import Eval.Exec
import Eval.Builtins
import Debug.Trace
import Eval.Instructions
import Ast.Operable (convOperation, concatInner, convOperable)

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

convAst :: Ast -> Context -> LocalContext -> Either String Insts
convAst (AstStructure struct) c l = convStruct struct c l
convAst (AstOperation op) c l = fst <$> convOperation op c l

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
