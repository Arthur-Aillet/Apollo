module Eval.Builtins (
  execBuiltin,
  Builtin (..), Stack
) where

import Eval.Values (Value (..), Builtin (..))

type Stack = [Value];

execBuiltin :: Builtin -> Stack -> Either String Stack
execBuiltin _ [] = Left "Op on empty stack"
execBuiltin Add (x:y:xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Int (ix + iy):xs)
  _ -> Left "Error: Add on unsupported types"
execBuiltin Sub (x:y:xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Int (ix - iy):xs)
  _ -> Left "Error: Sub on unsupported types"
execBuiltin Mul (x:y:xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Int (ix * iy):xs)
  _ -> Left "Error: Mul on unsupported types"
execBuiltin Div (x:y:xs) = case (x, y) of
  (Int _, Int 0) -> Left "Error : division by 0"
  (Int ix, Int iy) -> Right (Int (div ix iy):xs)
  _ -> Left "Error: Div on unsupported types"
execBuiltin Eq (x:y:xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Bool ((==) ix iy):xs)
  (Bool bx, Bool by) -> Right (Bool ((==) bx by):xs)
  _ -> Left "Error: Eq on unsupported types"
execBuiltin Less (x:y:xs) = case (x, y) of
  (Int ix, Int iy) -> Right (Bool ((<) ix iy):xs)
  _ -> Left "Error: Eq on unsupported types"
execBuiltin _ _ = Left "Op on only one element"
