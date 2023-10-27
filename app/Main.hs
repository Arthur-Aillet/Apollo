module Main (main) where

import Ast.Compile (Binary (..), compile)
import Ast.Type
import Eval
import Prelude

createAbs :: Definition
createAbs =
  FuncDefinition
    "abs"
    ( Function
        [("n", TypeInt)]
        (Just TypeInt)
        ( AstStructure
            ( If
                (OpOperation $ CallStd Less [OpVariable "n", OpValue (AtomI 0)])
                (AstStructure $ Return $ OpOperation $ CallStd Multiplication [OpVariable "n", OpValue (AtomI (-1))])
                (AstStructure $ Return $ OpVariable "n")
            )
        )
    )

createFib :: Definition
createFib =
  FuncDefinition
    "fib"
    ( Function
        [("n", TypeInt)]
        (Just TypeInt)
        ( AstStructure
            ( If
                (OpOperation $ CallStd Eq [OpVariable "n", OpValue (AtomI 0)])
                (AstStructure $ Return $ OpValue (AtomI 0))
                ( AstStructure $
                    If
                      (OpOperation $ CallStd Eq [OpVariable "n", OpValue (AtomI 1)])
                      (AstStructure $ Return $ OpValue (AtomI 1))
                      ( AstStructure $
                          Return $
                            OpOperation $
                              CallStd
                                Addition
                                [ OpOperation $ CallFunc "fib" [OpOperation $ CallStd Subtraction [OpVariable "n", OpValue (AtomI 1)]],
                                  OpOperation $ CallFunc "fib" [OpOperation $ CallStd Subtraction [OpVariable "n", OpValue (AtomI 2)]]
                                ]
                      )
                )
            )
        )
    )

createSub :: Definition
createSub =
  FuncDefinition
    "sub"
    ( Function
        [("a", TypeInt), ("b", TypeInt)]
        (Just TypeInt)
        ( AstStructure $
            Return $
              OpOperation $
                CallStd
                  Subtraction
                  [ OpVariable "a",
                    OpVariable "b"
                  ]
        )
    )

createMain :: Definition
createMain =
  FuncDefinition
    "main"
    ( Function
        []
        (Just TypeInt)
        ( AstStructure $
            Return $
              OpOperation $
                CallFunc "fib" [OpValue (AtomI 14)]
        )
    )

createGcd :: Definition
createGcd =
  FuncDefinition
    "gcd"
    ( Function
        [("x", TypeInt), ("y", TypeInt)]
        (Just TypeInt)
        ( AstStructure
            ( If
                (OpOperation $ CallStd Eq [OpValue (AtomI 0), OpVariable "y"])
                (AstStructure $ Return $ OpVariable "x")
                ( AstStructure $
                    Return $
                      OpOperation $
                        CallFunc
                          "gcd"
                          [OpVariable "y", OpOperation (CallStd Modulo [OpVariable "x", OpVariable "y"])]
                )
            )
        )
    )

main :: IO ()
main =
  case compile [createMain, createFib] of
    Left a -> putStrLn a
    Right (Binary env main_func) -> do
      result <- exec env [] main_func []
      case result of
        Left a -> putStrLn a
        Right a -> print a
