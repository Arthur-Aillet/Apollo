{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- AST To Insts Contxt
-}

module Ast.Context (Index, Context (..), LocalContext (..), createCtx, createLocalContext, firstValidIndex) where

import Ast.Type
  ( Definition (..),
    Function (..),
    Type,
  )
import Data.HashMap.Lazy (HashMap, fromList, insert)

type Index = Int

newtype Context = Context (HashMap String (Index, [(String, Type)], Maybe Type))

type CurrentReturnType = (Maybe Type)
type Defined = Bool
type Variables = (HashMap String (Index, Type, Defined))

data LocalContext = LocalContext Variables CurrentReturnType

attachIndex :: [(String, Type)] -> Index -> [(String, (Index, Type, Defined))]
attachIndex [] _ = []
attachIndex ((str, t) : xs) acc = (str, (acc, t, True)) : attachIndex xs (acc + 1)

createCtx :: [Definition] -> Context -> Int -> Either String Context
createCtx (FuncDefinition "main" _ : xs) ctx nbr = createCtx xs ctx nbr
createCtx (FuncDefinition name (Function args rval _) : xs) (Context ctx) nbr =
  createCtx xs (Context $ insert name (nbr, args, rval) ctx) (nbr + 1)
createCtx [] ctx _ = Right ctx

createLocalContext :: [(String, Type)] -> Maybe Type -> Either String LocalContext
createLocalContext args output =
  Right $ LocalContext (fromList (attachIndex args 0)) output

firstValidIndex :: Variables -> Index
firstValidIndex = foldl (\var (idx, _, _) -> max var idx) 0
