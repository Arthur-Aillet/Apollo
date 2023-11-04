{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Exec
-}

module Eval.Exec (module Eval.Exec, module Eval.Atom, module Eval.Instructions, module Eval.Operator) where

import Debug.Trace
import Eval.Atom (Atom (..))
import Eval.Instructions (Func, History, Index, Instruction (..), Insts, moveForward)
import Eval.Operator (Operator (..), Stack, Value (..), execOperator)

type Env = [(Int, Func)]

type Args = [Value]

data Pointer = Pointer Index [Index] deriving (Show)

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
convertValToInt (_ : xs) = Nothing
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

execL :: Env -> Args -> Insts -> History -> Stack -> IO (Either String (Maybe Value))
--execL env args insts h stack = trace ("Fst : "++ show (head insts) ++"  Args > " ++ show args) exec env args insts h stack
execL = exec

exec :: Env -> Args -> Insts -> History -> Stack -> IO (Either String (Maybe Value))
exec env args ((Take nbr : xs)) h stack =
  execL env args xs (Take nbr : h) new_stack
  where
    new_stack = VList start : end
    (start, end) = splitAt nbr stack
exec env args ((PushD (VAtom val)) : xs) h stack =
  execL env args xs (PushD (VAtom val) : h) (VAtom val : stack)
exec env args ((PushI arg_index) : xs) h stack = case getElem arg_index args of
  Left err -> return $ Left err
  Right arg -> execL env args xs (PushI arg_index : h) (arg : stack)
exec env args ((Op op) : xs) h stack = do
  result <- execOperator stack op
  case result of
    Left err -> return $ Left err
    Right new_stack -> execL env args xs (Op op : h) new_stack
exec env args ((CallD func_index) : xs) h stack = case getElem func_index env of
  Left err -> return $ Left err
  Right (args_nbr, insts) -> do
    result <- execL env start insts [] []
    case result of
      Left err -> return $ Left err
      Right (Just val) -> execL env args xs (CallD func_index : h) (val : end)
      Right Nothing -> execL env args xs (CallD func_index : h) end
    where
      (start, end) = splitAt args_nbr stack
exec env args ((JumpIfFalse line) : xs) h (VAtom 0 : ys)
  | line >= 0 = case moveForward line xs of
      Left a -> return $ Left a
      Right (start, end) ->
        execL env args end (reverse start ++ JumpIfFalse line : h) ys
  | otherwise = case moveForward (line * (-1)) h of
      Left a -> return $ Left a
      Right (start, end) ->
        execL env args (reverse start ++ JumpIfFalse line : xs) end ys
exec env args ((JumpIfFalse a) : xs) h (_ : ys) =
  execL env args xs (JumpIfFalse a : h) ys
exec _ _ ((JumpIfFalse _) : _) _ [] =
  return $ Left "Error: JumpIf on empty stack"
exec env args ((Jump line) : xs) h stack
  | line >= 0 = case moveForward line xs of
      Left a -> return $ Left a
      Right (start, end) ->
        execL env args end (start ++ h) stack
  | otherwise = case moveForward (line * (-1)) h of
      Left a -> return $ Left a
      Right (start, end) ->
        execL env args (reverse start ++ Jump line : xs) end stack
exec env args (Store : xs) h (y : ys) =
  execL env (args ++ [y]) xs (Store : h) ys
exec _ _ (Store : _) _ [] = return $ Left "Error: Store with empty stack"
exec env args (ArrAssign idx : xs) h (VList x : y : ys) = case createPtr idx x of
  Left err -> return $ Left err
  Right ptr -> case assignToPtr args ptr y of
    Left err -> return $ Left err
    Right new_args -> execL env new_args xs (ArrAssign idx : h) ys
exec _ _ (ArrAssign _ : _) _ (VAtom _ : _) =
  return $ Left "ArrAssign take a list"
exec env args (Assign i : xs) h (y : ys) = case getElem i args of
  Left err -> return $ Left err
  Right _ ->
    execL env (take i args ++ [y] ++ drop (i + 1) args) xs (Assign i : h) ys
exec _ _ (Ret : _) _ (y : _) = return $ Right $ Just y
exec _ _ (Ret : _) _ _ = return $ Left "Error: Return with empty stack"
exec _ _ [] _ _ = return $ Right Nothing
exec _ _ (x : _) _ _ = return $ Left $ "Error: Undefined Yet: " ++ show x
