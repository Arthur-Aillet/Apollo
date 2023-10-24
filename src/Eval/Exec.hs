module Eval.Exec (module Eval.Exec) where

import Eval.Builtin (Args, Builtin (..), Env, Func, Instruction (..), Insts, Stack, Value (..), execBuiltin, moveForward)

absFunc :: Func
absFunc =
  [ PushArg 0,
    Push (Int 0),
    Push (Op Less),
    Call,
    JumpIfFalse 2,
    PushArg 0,
    Ret,
    PushArg 0,
    Push (Int (-1)),
    Push (Op Mul),
    Call,
    Ret
  ]

createEnv :: Env
createEnv = [(1, absFunc)]

exec :: Env -> Args -> Insts -> Stack -> Either String Value
exec env args (Push val : xs) stack = exec env args xs (val : stack)
exec env args (PushArg val : xs) stack = exec env args xs (args !! val : stack)
exec env args (Call : xs) (Op y : ys) = case execBuiltin y ys of
  Right new_stack -> exec env args xs new_stack
  Left err -> Left err
exec env _ (Call : xs) (Func func_id : stack) = exec env start (insts ++ xs) end
  where
    (start, end) = splitAt args_nbr stack
    (args_nbr, insts) = env !! func_id
exec _ _ (Call : _) (y : _) = Left ("Error: Call to " ++ show y ++ " impossible, not a function")
exec env args (JumpIfFalse line : xs) (y : ys) = case y of
  Bool True -> exec env args xs ys
  Bool False -> case moveForward line xs of
    Left a -> Left a
    Right valid -> exec env args valid ys
  _ -> Left "Error: if on other type than boolean"
exec _ _ (Ret : _) (x : _) = Right x
exec _ _ (Ret : _) _ = Left "Error: Return with empty stack"
exec _ _ [] _ = Left "Error: Missing return"
exec _ _ _ _ = Left "Error: Undefined Yet"
