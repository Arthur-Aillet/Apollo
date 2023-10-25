{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts
-}

module Ast.ToInsts (module Ast.ToInsts) where

import Ast.Type
import Atom.Atom (Atom (AtomC, AtomI))
import Data.HashMap.Lazy (HashMap, empty, fromList, fromListWith, insert, (!?))
import Debug.Trace
import Eval.Builtin

newtype Context = Context (HashMap String (Int, Function))

data LocalContext = LocalContext (HashMap String (Int, Type)) (Maybe Type)

data Binary = Binary Env Func deriving (Show)

createGcd :: DefinitionDefinition
createGcd =
  FuncDefinition
    "gcd"
    (Function
      [("x", TypeInt), ("y", TypeInt)]
      (Just TypeInt)
      (AstStructure
        (If
          (OpOperation $ CallStd Eq [OpValue (AtomI 0), OpVariable "y"])
          (AstStructure $ Return $ OpVariable "x")
          (AstStructure $
            Return $
              OpOperation $
                CallFunc
                  "gcd"
                  [OpVariable "y", OpOperation (CallStd Mod [OpVariable "x", OpVariable "y"])]
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
convAllFunc ((FuncDefinition "main" func) : xs) (Binary env []) ctx = case convFunc func ctx of
  Left err -> Left err
  Right func -> convAllFunc xs (Binary env func) ctx
convAllFunc ((VarDefinition _ _) : _) _ _ = Left "Error: Global Variables not supported yet"
convAllFunc ((FuncDefinition _ (Function args y z)) : xs) (Binary env funcs) ctx = case convFunc (Function args y z) ctx of
  Left err -> Left err
  Right func -> convAllFunc xs (Binary (env ++ [(length args, func)]) funcs) ctx
convAllFunc [] bin _ = Right bin

(+++) :: [a] -> [a] -> [a] -> [a]
(+++) a b c = a ++ b ++ c

(++++) :: [a] -> [a] -> [a] -> [a] -> [a]
(++++) a b c d = a ++ b ++ c ++ d

attachIndex :: [(String, Type)] -> Int -> [(String, (Int, Type))]
attachIndex [] _ = []
attachIndex ((str, t) : xs) acc = (str, (acc, t)) : attachIndex xs (acc + 1)

createLocalContext :: [(String, Type)] -> Maybe Type -> Either String LocalContext
createLocalContext args output = Right $ LocalContext (fromList (attachIndex args 0)) output

convFunc :: Function -> Context -> Either String Insts
convFunc (Function args output ast) ctx = case createLocalContext args output of
  Left err -> Left err
  Right local -> convAst ast ctx local

convStruct :: Structure -> Context -> LocalContext -> Either String Insts
convStruct Resolved _ _ = Left "Err: Resolved unsupported"
convStruct (Return ope) c l = case convOperable ope c l of
  Left err -> Left err
  Right insts -> Right $ insts ++ [Ret]
convStruct (If op ast_then ast_else) c l =
  (++++)
    <$> opera
    <*> jump
    <*> then_insts
    <*> else_insts
  where
    opera = convOperable op c l
    jump = Right [JumpIfFalse (length then_insts)]
    then_insts = convAst ast_then c l
    else_insts = convAst ast_else c l
convStruct (Single ast) _ _ = Left "Err: Single unsupported"
convStruct (Block asts vars) _ _ = Left "Err: Block unsupported"
convStruct (Sequence asts) _ _ = Left "Err: Sequence unsupported"

convOperable :: Operable -> Context -> LocalContext -> Either String Insts
convOperable (OpValue (AtomI val)) c l = Right [Push (Int val)]
convOperable (OpVariable name) c (LocalContext hash _) = case hash !? name of
  Nothing -> Left $ "Variable: " ++ name ++ " never defined"
  Just (index, _) -> Right [PushArg index]
convOperable (OpOperation op) c l = convOperation op c l
convOperable (OpIOPipe op) _ _ = Left "Err: OpIOPipe unsupported"
convOperable (OpValue _) c l = Left "Err: OpValue not int unsupported"

convOperation :: Operation -> Context -> LocalContext -> Either String Insts
convOperation _ _ _ = Left "Err: Operation unsupported"
-- convOperation (CallStd builtin ops) _ _ = Right $ ops ++ [Push (Op Eq), Call builtin]

  {-
  = Interrupt String -- Interrupt program flow
  | CallStd String [Operable] -- call a standard or builtin operation (x(y))
  | CallFunc String [Operable] -- call a function, exposes both inherent IOPipes (x(y))
  | CallSH String [Operable] -- syscall of builtin program ($x(y)), exposes both IOPipes
  | Pipe Operable Operable -- stdout mapped to stdin ({x.y}, {x <- y})
  -}
convAst :: Ast -> Context -> LocalContext -> Either String Insts
convAst (AstStructure struct) = convStruct struct
convAst (AstOperation op) = convOperation op
