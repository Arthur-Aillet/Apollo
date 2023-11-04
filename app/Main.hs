module Main (main) where

import Ast.CompileAST (Binary (..))
import Ast.Display (compile)
import Ast.Type
import Eval
import PreProcess
import System.Environment
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
                [ ( OpOperation $ CallStd Lt [OpVariable "n", OpValue (AtomI 0)],
                    AstStructure $ Return $ OpOperation $ CallStd Mul [OpVariable "n", OpValue (AtomI (-1))]
                  )
                ]
                (Just $ AstStructure $ Return $ OpVariable "n")
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
                [ ( OpOperation $ CallStd Eq [OpVariable "n", OpValue (AtomI 0)],
                    AstStructure $ Return $ OpValue (AtomI 0)
                  ),
                  ( OpOperation $ CallStd Eq [OpVariable "n", OpValue (AtomI 1)],
                    AstStructure $ Return $ OpValue (AtomI 1)
                  )
                ]
                ( Just $
                    AstStructure $
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
                [ ( OpOperation $ CallStd Eq [OpValue (AtomI 0), OpVariable "y"],
                    AstStructure $ Return $ OpVariable "x"
                  )
                ]
                ( Just $
                    AstStructure $
                      Return $
                        OpOperation $
                          CallFunc
                            "gcd"
                            [OpVariable "y", OpOperation (CallStd Mod [OpVariable "x", OpVariable "y"])]
                )
            )
        )
    )

createFst :: Definition
createFst =
  FuncDefinition
    "main"
    ( Function
        []
        (Just TypeBool)
        ( AstStructure $
            Sequence
              [ AstStructure $ Return $ OpOperation $ CallStd Or [OpValue (AtomI 3), OpVariable "res"],
                AstStructure $ Return $ OpOperation $ CallStd Or [OpValue (AtomI 3), OpVariable "res"]
              ]
        )
    )

createSnd :: Definition
createSnd =
  FuncDefinition
    "main"
    ( Function
        []
        (Just TypeBool)
        ( AstStructure $
            Sequence
              [ AstStructure $ Return $ OpOperation $ CallStd Or [OpValue (AtomI 3), OpVariable "res"],
                AstStructure $ Return $ OpOperation $ CallStd Or [OpVariable "res", OpValue (AtomI 3), OpVariable "res"]
              ]
        )
    )

createCoolPrint :: Definition
createCoolPrint =
  FuncDefinition
    "cool_print"
    ( Function
        []
        Nothing
        (AstOperation $ CallStd Print [OpOperation $ CallStd Concat [OpList [OpValue (AtomC 't' True), OpValue (AtomC 'e' True), OpValue (AtomC 's' True), OpValue (AtomC 't' True), OpValue (AtomC '\n' True)], OpList [OpValue (AtomC 't' True), OpValue (AtomC 'e' True), OpValue (AtomC 's' True), OpValue (AtomC 't' True), OpValue (AtomC '\n' True)]]])
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
              [ AstStructure $ VarDefinition "arr" (TypeList (Just $ TypeList $ Just TypeChar)) (Just $ OpList [OpList [OpValue (AtomC 'a' True), OpValue (AtomC 'b' True), OpValue (AtomC 'c' True)], OpList [OpValue (AtomC 'd' True), OpValue (AtomC 'e' True), OpValue (AtomC 'f' True)]]),
                AstStructure $
                  For "i" (OpVariable "arr") $
                    AstStructure $
                      Sequence
                        [ AstOperation $ CallStd Print [OpVariable "i"]
                        ],
                AstStructure $ Return $ OpOperation $ CallStd Len [OpVariable "arr"]
              ]
        )
    )

main :: IO ()
main = do
  args <- getArgs
  files <- readFiles args
  (Binary env main_f) <- compile [createMain, createCoolPrint, createFib]
  result <- exec env [] main_f [] []
  case result of
    Left a -> putStrLn a
    Right a -> print a
