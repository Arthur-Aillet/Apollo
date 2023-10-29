{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Exec
-}

module Eval.Exec (module Eval.Exec, module Eval.Atom, module Eval.Instructions, module Eval.Operator) where

import Eval.Atom (Atom (..))
import Eval.Instructions (Func, History, Index, Instruction (..), Insts, moveForward)
import Eval.Operator (Operator (..), Stack, Value (..), execOperator)

type Env = [(Int, Func)]

type Args = [Value]

getElem :: Index -> [a] -> Either String a
getElem _ [] = Left "Error: Function args list empty"
getElem nb list
  | nb >= length list = Left "Error: Element asked outside args list"
  | nb < 0 = Left "Error: Element asked invalid"
  | otherwise = Right $ last $ take (nb + 1) list

exec :: Env -> Args -> Insts -> History -> Stack -> IO (Either String Value)
exec env args ((PushD val) : xs) h stack =
  exec env args xs (PushD val : h) (VAtom val : stack)
exec env args ((PushI arg_index) : xs) h stack = case getElem arg_index args of
  Left err -> return $ Left err
  Right arg -> exec env args xs (PushI arg_index : h) (arg : stack)
exec env args ((Op op) : xs) h stack = case execOperator stack op of
  Left err -> return $ Left err
  Right new_stack -> exec env args xs (Op op : h) new_stack
exec env args ((CallD func_index) : xs) h stack = case getElem func_index env of
  Left err -> return $ Left err
  Right (args_nbr, insts) -> do
    result <- exec env start insts [] []
    case result of
      Left err -> return $ Left err
      Right value -> exec env args xs (CallD func_index : h) (value : end)
    where
      (start, end) = splitAt args_nbr stack
exec env args ((JumpIfFalse line) : xs) h (VAtom 0 : ys)
  | line >= 0 = case moveForward line xs of
    Left a -> return $ Left a
    Right (start, end) -> exec env args end (start ++ h) ys
  | otherwise = case moveForward (line * (-1)) h of
    Left a -> return $ Left a
    Right (start, end) ->
      exec env args (reverse start ++ JumpIfFalse line : xs) end ys
exec env args ((JumpIfFalse a) : xs) h (_ : ys) =
  exec env args xs (JumpIfFalse a : h) ys
exec env args ((Jump line) : xs) h stack
  | line >= 0 = case moveForward line xs of
    Left a -> return $ Left a
    Right (start, end) -> exec env args end (start ++ h) stack
  | otherwise = case moveForward (line * (-1)) h of
    Left a -> return $ Left a
    Right (start, end) ->
      exec env args (reverse start ++ Jump line : xs) end stack
exec env args (Store : xs) h (y : ys) = exec env (args ++ [y]) xs (Store : h) ys
exec env args (Assign i : xs) h (y : ys) = case getElem i args of
  Left err -> return $ Left err
  Right _ ->
    exec env (take i args ++ [y] ++ drop (i + 1) args) xs (Assign i : h) ys
exec _ _ (Ret : _) _ (y : _) = return $ Right y
exec _ _ (Ret : _) _ _ = return $ Left "Error: Return with empty stack"
exec _ _ [] _ _ = return $ Left "Error: Missing return"
exec _ _ _ _ _ = return $ Left "Error: Undefined Yet"
