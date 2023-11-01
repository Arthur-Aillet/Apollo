module Main (main) where

import Ast.Compile (Binary (..))
import Ast.Display (compile)
import Ast.Error (Compile (..))
import Ast.Type
import Eval
import Eval.Exec (Operator (Add, Sub))
import System.Exit (ExitCode (ExitFailure), exitWith)
import Prelude
import Parser.Type (Parser(..), StackTrace)
import System.Console.Haskeline
    ( getInputLine,
      completeWord,
      simpleCompletion,
      runInputT,
      Completion,
      InputT,
      Settings(Settings, autoAddHistory, complete, historyFile) )
import System.IO (BufferMode (..), hGetContents', hIsTerminalDevice, hSetBuffering, stdin, stdout)
import Control.Monad.IO.Class
import Data.HashMap.Internal.Strict (keys)
import Data.List (isPrefixOf)
-- import Parser.String (parseStringWithHandleBackslash)
import Parser.Condition(parseOperation)
import Parser.Position(defaultPosition)

keywords :: [String]
keywords = []

search :: [String] -> String -> [Completion]
search symbols str = map simpleCompletion $
    filter (str `isPrefixOf`) (keywords ++ symbols)

inputKey :: String
inputKey = "\ESC[33mApollo\ESC[28m> "

haskelineGetline :: InputT IO String
haskelineGetline = do
                    input <- getInputLine inputKey
                    case input of
                      Nothing -> return ""
                      Just str -> return str

newSettings ::  MonadIO m => Settings m
newSettings = Settings {
                  complete = completeWord Nothing " \t" $
                    return . search [],
                  historyFile = Just ".history",
                  autoAddHistory = True
                }

getInstructions :: IO ()
getInstructions = do
  new_line <- runInputT newSettings haskelineGetline
  case runParser parseOperation new_line defaultPosition of
    Right (i, str, pos) -> do
      print(new_line)
      print(i)
      print(str)
      getInstructions
    Left a -> do
      print a

-- createAbs :: Definition
-- createAbs =
--   FuncDefinition
--     "abs"
--     ( Function
--         [("n", TypeInt)]
--         (Just TypeInt)
--         ( AstStructure
--             ( If
--                 (OpOperation $ CallStd Lt [OpVariable "n", OpValue (AtomI 0)])
--                 (AstStructure $ Return $ OpOperation $ CallStd Mul [OpVariable "n", OpValue (AtomI (-1))])
--                 (AstStructure $ Return $ OpVariable "n")
--             )
--         )
--     )

-- createFib :: Definition
-- createFib =
--   FuncDefinition
--     "fib"
--     ( Function
--         [("n", TypeInt)]
--         (Just TypeInt)
--         ( AstStructure
--             ( If
--                 (OpOperation $ CallStd Eq [OpVariable "n", OpValue (AtomI 0)])
--                 (AstStructure $ Return $ OpValue (AtomI 0))
--                 ( AstStructure $
--                     If
--                       (OpOperation $ CallStd Eq [OpVariable "n", OpValue (AtomI 1)])
--                       (AstStructure $ Return $ OpValue (AtomI 1))
--                       ( AstStructure $
--                           Return $
--                             OpOperation $
--                               CallStd
--                                 Add
--                                 [ OpOperation $ CallFunc "fib" [OpOperation $ CallStd Sub [OpVariable "n", OpValue (AtomI 1)]],
--                                   OpOperation $ CallFunc "fib" [OpOperation $ CallStd Sub [OpVariable "n", OpValue (AtomI 2)]]
--                                 ]
--                       )
--                 )
--             )
--         )
--     )

-- createSub :: Definition
-- createSub =
--   FuncDefinition
--     "sub"
--     ( Function
--         [("a", TypeInt), ("b", TypeInt)]
--         (Just TypeInt)
--         ( AstStructure $
--             Return $
--               OpOperation $
--                 CallStd
--                   Sub
--                   [ OpVariable "a",
--                     OpVariable "b"
--                   ]
--         )
--     )

-- createGcd :: Definition
-- createGcd =
--   FuncDefinition
--     "gcd"
--     ( Function
--         [("x", TypeInt), ("y", TypeInt)]
--         (Just TypeInt)
--         ( AstStructure
--             ( If
--                 (OpOperation $ CallStd Eq [OpValue (AtomI 0), OpVariable "y"])
--                 (AstStructure $ Return $ OpVariable "x")
--                 ( AstStructure $
--                     Return $
--                       OpOperation $
--                         CallFunc
--                           "gcd"
--                           [OpVariable "y", OpOperation (CallStd Mod [OpVariable "x", OpVariable "y"])]
--                 )
--             )
--         )
--     )

-- createMain :: Definition
-- createMain =
--   FuncDefinition
--     "main"
--     ( Function
--         []
--         (Just TypeInt)
--         ( AstStructure $
--             Sequence
--               [ AstStructure $ VarDefinition "res" TypeInt (Just $ OpValue (AtomI 10)),
--                 AstStructure $
--                   While
--                     (OpOperation $ CallStd Gt [OpVariable "res", OpValue (AtomI (-5))])
--                     (AstStructure $ VarAssignation "res" $ OpOperation $ CallStd Sub [OpVariable "res", OpValue (AtomI 1)]),
--                 AstStructure $ Return $ OpVariable "res"
--               ]
--         )
--     )

-- main :: IO ()
-- main = do
--   (Binary env main_f) <- compile [createMain, createAbs]
--   result <- exec env [] main_f [] []
--   case result of
--     Left a -> putStrLn a
--     Right a -> print a

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  bool <- hIsTerminalDevice stdin
  if bool
    then getInstructions
    else do
      content <- hGetContents' stdin
      print(content)