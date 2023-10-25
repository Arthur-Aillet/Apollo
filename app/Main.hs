module Main (main) where

import Data.Either (Either (Right))
import Eval
import Prelude

main :: IO ()
main = case exec createEnv [] [Push (Int (-42)), Push (Func "abs"), Call, Ret] [] of
  Left a -> putStrLn a
  Right a -> print a
