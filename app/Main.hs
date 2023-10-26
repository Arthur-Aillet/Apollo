module Main (main) where

import Ast.Compile (Binary (..), compile, createGcd, createMain)
import Eval
import Prelude

main :: IO ()
main =
  case compile [createMain, createGcd] of
    Left a -> putStrLn a
    Right (Binary env main_func) -> do
      print env
      print main_func
      result <- exec env [] main_func []
      case result of
        Left a -> putStrLn a
        Right a -> print a
