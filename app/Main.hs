module Main (main) where

import Eval
import Prelude

main :: IO ()
main = case exec createEnv [] [PushD (AtomF (-42.5)), CallD "abs", Ret] [] of
    Left a -> putStrLn a
    Right a -> print a
