{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Exec
-}

module Eval.Exec (module Eval.Exec, module Eval.Atom, module Eval.Instructions, module Eval.Operator) where

import Eval.Atom (Atom (..), Type (..), bAtom, cAtom, iAtom, fAtom)
import Eval.Instructions (Func, History, Index, Instruction (..), Insts, moveForward, Machine, Pointer(..), Env, Args)
import Eval.Operator (Operator (..), Stack, Value (..), execOperator)
import Eval.Syscall (execSys)

getElem :: Index -> [a] -> Either String a
getElem _ [] = Left "Error: Function args list empty"
getElem nb list
  | nb >= length list = Left "Error: Element asked outside args list"
  | nb < 0 = Left "Error: Element asked invalid"
  | otherwise = Right $ last $ take (nb + 1) list

convertValToInt :: [Value] -> Maybe [Index]
convertValToInt (VAtom (AtomI idx) : xs) = case convertValToInt xs of
  Just arr -> Just $ idx : arr
  Nothing -> Nothing
convertValToInt (_ : _) = Nothing
convertValToInt [] = Just []

setElem :: Index -> [a] -> a -> Either String [a]
setElem idx list new = case getElem idx list of
  Left err -> Left err
  Right _ -> Right new_args
  where
    new_args = start ++ (new : tail end)
    (start, end) = splitAt idx list

modifyInArr :: Value -> [Index] -> Value -> Either String Value
modifyInArr _ [] new_val = Right new_val
modifyInArr (VAtom _) _ _ = Left "Can't access inside non int"
modifyInArr (VList arr) (x : xs) new_val = case getElem x arr of
  Left err -> Left err
  Right old -> case modifyInArr old xs new_val of
    Left err -> Left err
    Right new -> Right (VList $ start ++ (new : tail end))
  where
    (start, end) = splitAt x arr

assignToPtr :: Args -> Pointer -> Value -> Either String Args
assignToPtr args (Pointer pos idx) new_val = case getElem pos args of
  Left err -> Left err
  Right old_arr -> case modifyInArr old_arr idx new_val of
    Left err -> Left err
    Right new_arr -> Right $ start ++ (new_arr : tail end)
  where
    (start, end) = splitAt pos args

createPtr :: Index -> [Value] -> Either String Pointer
createPtr arr vals = case convertValToInt vals of
  Nothing -> Left "Indexes in pointer need to be ints"
  Just idxs -> Right $ Pointer arr idxs

execInstr :: Machine -> Either String Machine
execInstr (env, args, Take nbr : xs, h, stack) =
  Right (env, args, xs, Take nbr : h, new_stack)
  where
    new_stack = VList start : end
    (start, end) = splitAt nbr stack

execInstr (env, args, (PushD val) : xs, h, stack) =
  Right (env, args, xs, PushD val : h, VAtom val : stack)

execInstr (env, args, (PushI arg_index) : xs, h, stack) =
  case getElem arg_index args of
    Left err -> Left err
    Right arg -> Right (env, args, xs, PushI arg_index : h, arg : stack)

execInstr (env, args, (Op op) : xs, h, stack) =
  case execOperator stack op of
    Left err -> Left err
    Right new_stack -> Right (env, args, xs, Op op : h, new_stack)

execInstr (env, args, (Cast t) : xs, h, y : stack) =
  case y of
    (VAtom x) -> case t of
      TypeBool -> Right (env, args, xs, Cast t : h, VAtom (bAtom x) : stack)
      TypeChar ->Right (env, args, xs, Cast t : h, VAtom (cAtom x) : stack)
      TypeInt -> Right (env, args, xs, Cast t : h, VAtom (iAtom x) : stack)
      TypeFloat -> Right (env, args, xs, Cast t : h, VAtom (fAtom x) : stack)
    _ -> Left "error: cannot cast non-primitive"

execInstr (env, args, (JumpIfFalse line) : xs, h, VAtom 0 : ys)
  | line >= 0 = case moveForward line xs of
      Left a -> Left a
      Right (start, end) ->
        Right (env, args, end, reverse start ++ JumpIfFalse line : h, ys)
  | otherwise = case moveForward (line * (-1)) h of
      Left a -> Left a
      Right (start, end) ->
        Right (env, args, reverse start ++ JumpIfFalse line : xs, end, ys)

execInstr (env, args, (JumpIfFalse a) : xs, h, _ : ys) =
  Right (env, args, xs, JumpIfFalse a : h, ys)

execInstr (_, _, (JumpIfFalse _) : _, _, []) =
  Left "Error: JumpIf on empty stack"

execInstr (env, args, (Jump line) : xs, h, stack)
  | line >= 0 = case moveForward line xs of
      Left a -> Left a
      Right (start, end) ->
        Right (env, args, end, start ++ h, stack)
  | otherwise = case moveForward (line * (-1)) h of
      Left a -> Left a
      Right (start, end) ->
        Right (env, args, reverse start ++ Jump line : xs, end, stack)

execInstr (env, args, Store : xs, h, y : ys) =
  Right (env, args ++ [y], xs, Store : h, ys)

execInstr (_, _, Store : _, _, []) = Left "Error: Store with empty stack"

execInstr (env, args, ArrAssign idx : xs, h, VList x : y : ys) =
  case createPtr idx x of
    Left err -> Left err
    Right ptr -> case assignToPtr args ptr y of
      Left err -> Left err
      Right new_args -> Right (env, new_args, xs, ArrAssign idx : h, ys)

execInstr (_, _, ArrAssign _ : _, _, VAtom _ : _) =
  Left "ArrAssign take a list"

execInstr (env, args, Assign i : xs, h, y : ys) = case getElem i args of
  Left err -> Left err
  Right _ ->
    Right (env, newargs, xs, Assign i : h, ys)
  where
    newargs = take i args ++ [y] ++ drop (i + 1) args

execInstr (_, _, Ret : _, _, _) = Left "Error: Return with empty stack"

execInstr (_, _, x : _, _, _) = Left $ "Error: Undefined Yet: " ++ show x

execInstr (_, _, [], _, _) = Left "Error: End of Tape"


exec :: Machine -> IO (Either String (Maybe Value))
exec (_, _, Ret : _, _, y : _) = return $ Right $ Just y
exec (env, args, (CallD f_index) : xs, h, stack) =
  case getElem f_index env of
    Left err -> return $ Left err
    Right (args_nbr, insts) -> do
      result <- exec (env, start, insts, [], [])
      case result of
        Left err -> return $ Left err
        Right (Just val) -> exec (env, args, xs, CallD f_index : h, val : end)
        Right Nothing -> exec (env, args, xs, CallD f_index : h, end)
      where (start, end) = splitAt args_nbr stack
exec (env, args, (Sys call) : xs, h, stack) = do
  result <- execSys stack call
  case result of
    Left err -> return $ Left err
    Right _ -> exec (env, args, xs, Sys call : h, stack)
exec (env, args, xs, h, stack) =
  case execInstr (env, args, xs, h, stack) of
    Left err -> return $ Left err
    Right machine -> exec machine
