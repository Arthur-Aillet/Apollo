
import Ast.Compile (Binary (..))
import Ast.Display (compile)
-- import Ast.Error (Compile (..))
-- import Ast.Type
-- import System.Console.Haskeline
--     ( getInputLine,
--       completeWord,
--       simpleCompletion,
--       runInputT,
--       Completion,
--       InputT,
--       Settings(Settings, autoAddHistory, complete, historyFile) )
-- import Control.Monad.IO.Class
-- import Data.HashMap.Internal.Strict (keys)
-- import Data.List (isPrefixOf)
import Eval

import PreProcess
import System.Environment
-- import System.Exit (ExitCode (ExitFailure), exitWith)
import Prelude
-- import Ast.Type (Definition (FuncDefinition), Function (Function), Ast (AstOperation), Operation (CallStd), Operable (OpOperation))

-- keywords :: [String]
-- keywords = []

-- search :: [String] -> String -> [Completion]
-- search symbols str = map simpleCompletion $
--     filter (str `isPrefixOf`) (keywords ++ symbols)

-- inputKey :: String
-- inputKey = "\ESC[33mApollo\ESC[28m> "

-- haskelineGetline :: InputT IO String
-- haskelineGetline = do
--                     input <- getInputLine inputKey
--                     case input of
--                       Nothing -> return ""
--                       Just str -> return str

-- newSettings ::  MonadIO m => Settings m
-- newSettings = Settings {
--                   complete = completeWord Nothing " \t" $
--                     return . search [],
--                   historyFile = Just ".history",
--                   autoAddHistory = True
--                 }

-- getInstructions :: IO ()
-- getInstructions = do
--   new_line <- runInputT newSettings haskelineGetline
--   case runParser parseCondOperation new_line defaultPosition of
--     Right (i, str, pos) -> do
--       print(new_line)
--       print(i)
--       print(str)
--       getInstructions
--     Left a -> do
--       print a

-- createCoolPrint :: Definition
-- createCoolPrint =
--   FuncDefinition
--     "cool_print"
--     ( Function
--         []
--         Nothing
--         (AstOperation $ CallStd Print [OpOperation $ CallStd Concat [OpList [OpValue (AtomC 't' True), OpValue (AtomC 'e' True), OpValue (AtomC 's' True), OpValue (AtomC 't' True), OpValue (AtomC '\n' True)], OpList [OpValue (AtomC 't' True), OpValue (AtomC 'e' True), OpValue (AtomC 's' True), OpValue (AtomC 't' True), OpValue (AtomC '\n' True)]]])
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
--               [ AstStructure $ VarDefinition "arr" (TypeList (Just TypeChar)) (Just $ OpList [OpValue (AtomC 't' True), OpValue (AtomC 'e' True), OpValue (AtomC 's' True), OpValue (AtomC 't' True), OpValue (AtomC '\n' True)]),
--                 AstStructure $ ArrAssignation "arr" [OpValue (AtomI 2)] $ OpValue (AtomC '3' True),
--                 AstOperation $ CallStd Print [OpVariable "arr"],
--                 AstStructure $ Return $ OpOperation $ CallStd Len [OpVariable "arr"]
--               ]
--         )
--     )

-- main :: IO ()
-- main = do
--   args <- getArgs
--   files <- readFiles args
--   (Binary env main_f) <- compile [createMain, createCoolPrint, createFib]
--   result <- exec env [] main_f [] []
--   case result of
--     Left a -> putStrLn a
--     Right a -> print a

main :: IO ()
main = pure()
