module Main (main) where

import Ast.Compile (Binary (..))
import Ast.Display (compile)
import Ast.Error (Compile (..))
import Ast.Type
import Eval
import Eval.Exec (Operator (Add, Sub))
import System.Exit (ExitCode (ExitFailure), exitWith)
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
                (OpOperation $ CallStd Lt [OpVariable "n", OpValue (AtomI 0)])
                (AstStructure $ Return $ OpOperation $ CallStd Mul [OpVariable "n", OpValue (AtomI (-1))])
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
                                Add
                                [ OpOperation $ CallFunc "fib" [OpOperation $ CallStd Sub [OpVariable "n", OpValue (AtomI 1)]],
                                  OpOperation $ CallFunc "fib" [OpOperation $ CallStd Sub [OpVariable "n", OpValue (AtomI 2)]]
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
                  Sub
                  [ OpVariable "a",
                    OpVariable "b"
                  ]
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
                          [OpVariable "y", OpOperation (CallStd Mod [OpVariable "x", OpVariable "y"])]
                )
            )
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
            Sequence
              [ AstStructure $ VarDefinition "res" TypeInt (Just $ OpValue (AtomI 10)),
                AstStructure $
                  While
                    (OpOperation $ CallStd Gt [OpVariable "res", OpValue (AtomI (-5))])
                    (AstStructure $ VarAssignation "res" $ OpOperation $ CallStd Sub [OpVariable "res", OpValue (AtomI 1)]),
                AstStructure $ Return $ OpVariable "res"
              ]
        )
    )

main :: IO ()
main = do
  (Binary env main_f) <- compile [createMain, createAbs]
  result <- exec env [] main_f [] []
  case result of
    Left a -> putStrLn a
    Right a -> print a
