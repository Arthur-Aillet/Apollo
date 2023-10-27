--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Exec
--

module Eval.Exec (module Eval.Exec, module Eval.Atom, module Eval.Instructions, module Eval.Operator) where

import Debug.Trace (trace)
import Eval.Atom (Atom (..))
import Eval.Instructions (Func, Index, Instruction (..), Insts, moveForward)
import Eval.Operator (Operator (..), Stack, execOperator)

type Env = [(Int, Func)]

type Args = [Atom]

absFunc :: Func
absFunc =
  [ PushI 0,
    PushD (AtomI 0),
    Op Less,
    JumpIfFalse 2,
    PushI 0,
    Ret,
    PushI 0,
    PushD (AtomI (-1)),
    Op Multiplication,
    Ret
  ]

createEnv :: Env
createEnv = [(1, absFunc)]

getElem :: Index -> [a] -> Either String a
getElem _ [] = Left "Error: Function args list empty"
getElem nb list
  | nb >= length list = Left "Error: Element asked outside args list"
  | nb < 0 = Left "Error: Element asked invalid"
  | otherwise = Right (last (take (nb + 1) list))

exec :: Env -> Args -> Insts -> Stack -> IO (Either String Atom)
exec env args ((PushD val) : xs) stack = exec env args xs (val : stack)
exec env args ((PushI arg_index) : xs) stack = case getElem arg_index args of
  Left err -> return $ Left err
  Right arg -> exec env args xs (arg : stack)
exec env args ((Op op) : xs) stack = case execOperator stack op of
  Left err -> return $ Left err
  Right new_stack -> exec env args xs new_stack
exec env args ((CallD func_index) : xs) stack = case getElem func_index env of
  Left err -> return $ Left err
  Right (args_nbr, insts) -> do
    result <- exec env start insts []
    case result of
      Left err -> return $ Left err
      Right atom -> exec env args xs (atom : end)
    where
      (start, end) = splitAt args_nbr stack
exec env args ((JumpIfFalse line) : xs) (y : ys) =
  if y == 0
    then case moveForward line xs of
      Left a -> return $ Left a
      Right valid -> exec env args valid ys
    else exec env args xs ys
exec _ _ (Ret : _) (y : _) = return $ Right y
exec _ _ (Ret : _) _ = return $ Left "Error: Return with empty stack"
exec _ _ [] _ = return $ Left "Error: Missing return"
exec _ _ _ _ = return $ Left "Error: Undefined Yet"
