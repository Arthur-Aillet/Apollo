module Main (main) where

import Ast.CompileAST (Binary (..), generateBinary)
import Ast.Display (compile)
import Ast.Error (Compile (..))
import Ast.Type
import Eval
import Eval.Exec
import Eval.Exec (Operator (Add, Or, Sub))
import PreProcess
import System.Environment
import System.Exit (ExitCode (ExitFailure), exitWith)
import Prelude
import Parser.Type (Parser(..), StackTrace)
import Control.Monad.IO.Class
import Data.HashMap.Internal.Strict (keys)
import Data.List (isPrefixOf)
import Parser.Position(defaultPosition)
import Parser.Parser(parser)

main :: IO ()
main = do
  args <- getArgs
  files <- readFiles args
  defs <- parser files
  print "ast: \n"
  print defs
  print "\n"
  (Binary env main_f) <- compile defs
  print "bin:\n"
  print env
  print "\n"
  print main_f
  print "\n"
  -- result <- exec env [] main_f [] []
  pure()

