{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts
-}

module Ast.ToInsts (module Ast.ToInsts) where

import Eval.Builtin
import Ast.Type
import Atom.Atom ( Atom (AtomI) )

import Data.HashMap.Lazy (empty, (!?), insert, HashMap)

newtype Context = Context (HashMap String (Int, Function))

data Binary = Binary Env Func deriving (Show)

createGcd :: Definition
createGcd = FuncDefinition "gcd" (Function [("x", TypeInt), ("y", TypeInt)] (Just TypeInt) (
    AstStructure
     (If (OpOperation (CallStd "==" [OpValue (AtomI 0), OpVariable "y"]))
        (AstStructure $ StructResult $ OpVariable "x")
        (AstStructure $ StructResult $ OpOperation $ CallFunc "gcd"
            [OpVariable "y", OpOperation (CallStd "%" [OpVariable "x", OpVariable "y"])]
        )
      )
    ))

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
convFunc (Function args output ast) = []
