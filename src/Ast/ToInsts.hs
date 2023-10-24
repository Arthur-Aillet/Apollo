{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts
-}

module Ast.ToInsts () where

import Eval.Builtin
import Ast.Type ( Function(..), Ast(..), Definition(..), Type )
import Atom.Atom ( Atom )

import Data.HashMap.Lazy (empty, (!?), insert, HashMap)

newtype Context = Context (HashMap String (Int, Function))

data Binary = Binary Env Func

createCtx :: [Definition] -> Context -> Int -> Either String Context
createCtx (FuncDefinition string def:xs) (Context ctx) nbr = 
    createCtx xs (Context $ insert string (nbr, def) ctx) (nbr + 1)
createCtx (VarDefinition _ _:_) _ _ = Left "Error: Global Variables not supported yet"
createCtx [] ctx _ = Right ctx

toInsts :: [Definition] -> Either String Binary
toInsts defs = case createCtx defs (Context empty) 0 of
    Left str -> Left str
    Right ctx -> convAllFunc defs (Binary [] []) ctx

convAllFunc :: [Definition] -> Binary -> Context -> Either String Binary
convAllFunc ((FuncDefinition "main" func):xs) (Binary env []) ctx = 
    convAllFunc xs (Binary env (convFunc func)) ctx
convAllFunc ((VarDefinition _ _):_) _ _ = Left "Error: Global Variables not supported yet"
convAllFunc ((FuncDefinition _ (Function args y z)):xs) (Binary env funcs) ctx = 
    convAllFunc xs (Binary (env ++ [(length args, convFunc (Function args y z))]) funcs) ctx
convAllFunc [] bin _ = Right bin

convFunc :: Function -> Insts
convFunc a = []
