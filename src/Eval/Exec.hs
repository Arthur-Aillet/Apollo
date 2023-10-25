--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Exec
--

module Eval.Exec (module Eval.Exec, module Eval.Atom, module Eval.Instructions, module Eval.Builtins) where

import Eval.Atom (Atom (..))
import Eval.Instructions (Instruction (..), Insts, moveForward, Index, Func)
import Eval.Builtins (Operator (..), execOperator, Stack)

type Env = [(Int, Func)];
type Args = [Atom];

absFunc :: Func
absFunc = [
  PushI 0,
  PushD (AtomI 0),
  Op Less,
  JumpIfFalse 2,
  PushI 0,
  Ret,
  PushI 0,
  PushD (AtomI (-1)),
  Op Multiplication,
  Ret]

createEnv :: Env
createEnv = [(1, absFunc)]

getElem :: Index -> [a] -> Either String a
getElem _ [] = Left "Error: Function args list empty"
getElem nb list
  | nb >= length list = Left "Error: Element asked outside args list"
  | nb < 0 = Left "Error: Element asked invalid"
  | otherwise = Right (last (take (nb + 1) list))

exec :: Env -> Args -> Insts -> Stack -> Either String Atom
exec env args ((PushD val) : xs) stack = exec env args xs (val:stack)
exec env args ((PushI arg_index) : xs) stack = case getElem arg_index args of
  Left err -> Left err
  Right arg -> exec env args xs (arg:stack)
exec env args ((Op op) : xs) stack = case execOperator stack op of
  Right new_stack -> exec env args xs new_stack
  Left err -> Left err
exec env _ ((CallD func_index) : xs) stack = exec env start (insts ++ xs) end
  where
    (start, end) = splitAt args_nbr stack
    (args_nbr, insts) = env !! func_index
exec env args ((JumpIfFalse line) : xs) (y:ys) =
  if y == 0
  then case moveForward line xs of
      Left a -> Left a
      Right valid -> exec env args valid ys
  else exec env args xs ys
exec _ _ (Ret:_) (x:_) = Right x
exec _ _ (Ret:_) _ = Left "Error: Return with empty stack"
exec _ _ [] _ = Left "Error: Missing return"
exec _ _ _ _ = Left "Error: Undefined Yet"
