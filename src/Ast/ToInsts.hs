{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts
-}

module Ast.ToInsts (module Ast.ToInsts) where

import Ast.Type
import Atom.Atom (Atom (AtomI))
import Data.HashMap.Lazy (HashMap, empty, fromList, insert, (!?))
import Debug.Trace
import Eval.Builtin

newtype Context = Context (HashMap String (Int, Function))

data LocalContext = LocalContext (HashMap String Type) (Maybe Type)

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
                (OpOperation (CallStd "==" [OpValue (AtomI 0), OpVariable "y"]))
                (AstStructure $ Return $ OpVariable "x")
                ( AstStructure $
                    Return $
                      OpOperation $
                        CallFunc
                          "gcd"
                          [OpVariable "y", OpOperation (CallStd "%" [OpVariable "x", OpVariable "y"])]
                )
            )
        )
    )

createCtx :: [Definition] -> Context -> Int -> Either String Context
createCtx (FuncDefinition string def : xs) (Context ctx) nbr =
  createCtx xs (Context $ insert string (nbr, def) ctx) (nbr + 1)
createCtx (VarDefinition _ _ : _) _ _ = Left "Error: Global Variables not supported yet"
createCtx [] ctx _ = Right ctx

toInsts :: [Definition] -> Either String Binary
toInsts defs = case createCtx defs (Context empty) 0 of
  Left str -> Left str
  Right ctx -> convAllFunc defs (Binary [] []) ctx

convAllFunc :: [Definition] -> Binary -> Context -> Either String Binary
convAllFunc ((FuncDefinition "main" func) : xs) (Binary env []) ctx =
  convAllFunc xs (Binary env (convFunc func ctx)) ctx
convAllFunc ((VarDefinition _ _) : _) _ _ = Left "Error: Global Variables not supported yet"
convAllFunc ((FuncDefinition _ (Function args y z)) : xs) (Binary env funcs) ctx =
  convAllFunc xs (Binary (env ++ [(length args, convFunc (Function args y z) ctx)]) funcs) ctx
convAllFunc [] bin _ = Right bin

createLocalContext :: [(String, Type)] -> Maybe Type -> LocalContext
createLocalContext args = LocalContext (fromList args)

convFunc :: Function -> Context -> Insts
convFunc (Function args output ast) ctx = convAst ast ctx local
  where
    local = createLocalContext args output

convStruct :: Structure -> Context -> LocalContext -> Insts
convStruct Resolved _ _ = []
convStruct (Return a) _ _ = []
convStruct (If op ast_then ast_else) c l =
    opera
    ++ jump
    ++ then_insts
    ++ else_insts
  where
    opera = convOp op c l
    jump = [JumpIfFalse (length then_insts)]
    then_insts = convAst ast_then c l
    else_insts = convAst ast_else c l
convStruct (Single ast) _ _ = []
convStruct (Block asts vars) _ _ = []
convStruct (Sequence asts) _ _ = []

convOp :: Operable -> Context -> LocalContext -> Insts
convOp _ _ _ = [Push (Int 0)]

convAst :: Ast -> Context -> LocalContext -> Insts
convAst (AstStructure struct) c l = convStruct struct c l
convAst (AstOperation op) _ _ = []
