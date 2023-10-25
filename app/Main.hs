module Main (main) where

import Data.Either (Either (Right))
import Eval
import Prelude

main :: IO ()
main = do
    result <- exec createEnv [] [PushD (AtomF (-42.5)), CallD 0, Ret] []
    case result of
        Left a -> putStrLn a
        Right a -> print a
