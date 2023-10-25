module Main (main) where

import Data.Either (Either (Right))
import Eval
import Prelude

main :: IO ()
main = case exec createEnv [] [PushD (AtomI (-42)), CallD 0, Ret] [] of
  Left a -> putStrLn a
  Right a -> print a
