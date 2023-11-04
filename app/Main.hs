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

defaultHelp :: String
defaultHelp = "\
\Usage:\n\
\\t./apollo [\ESC[33mrun\ESC[0m [files] (-- [args]) | \ESC[33mbuild\ESC[0m [files] (-- [name])| \ESC[33mlaunch\ESC[0m [binary] (-- [args])]\n\
\use\n\
\  ./apollo -h [\ESC[33mrun\ESC[0m | \ESC[33mbuild\ESC[0m | \ESC[33mlaunch\ESC[0m]\n\
\for more details about these commands\n\
\"

runHelp :: String
runHelp = "\
\Compiles and execute the files with given args\n\
\Usage:\n\
\\t./apollo run \ESC[33m[files]\ESC[0m (-- \ESC[33m[args]\ESC[0m)\n\
\\ESC[33mfiles\ESC[0m\t\tlist of files to execute\n\
\\ESC[33margs\ESC[0m\t\targuments to execute with.\
\if none are provided \"--\" is unnecessary\n\
\"

buildHelp :: String
buildHelp = "\
\Compiles the ginven files in a binary\n\
\Usage:\n\
\\t./apollo build \ESC[33m[files]\ESC[0m\ESC[0m (-- \ESC[33m[name]\ESC[0m)\n\
\\ESC[33mfiles\ESC[0m\t\tlist of files to execute\n\
\\ESC[33mname\ESC[0m\t\tthe name to give the binary.\
\if none is given, it will be named a.out and \"--\" is unnecessary\n\
\"

launchHelp :: String
launchHelp = "\
\Executes the given binary with given args\n\
\Usage:\n\
\\t./apollo launch \ESC[33m[binary]\ESC[0m) (-- \ESC[33m[args]\ESC[0m)\n\
\\ESC[33mbinary\ESC[0m\t\tbinary to execute\n\
\\ESC[33margs\ESC[0m\t\targuments to execute with.\
\if none are provided \"--\" is unnecessary\n\
\"

invalidHelp :: String
invalidHelp = "\
\Invalid -h arguments\n\
\use\n\
\  \ESC[33m./apollo -h\ESC[0m\n\
\for help\n\
\"

help :: [String] -> IO ()
help ([]) = putStr defaultHelp
help ("run" : []) = putStr runHelp
help ("build" : []) = putStr buildHelp
help ("launch" : []) = putStr launchHelp
help _ = putStr invalidHelp

getStrsBefore :: [String] -> String -> [String]
getStrsBefore (x : []) target | x == target = []
                              | otherwise = [x]
getStrsBefore (x : xs) target | x == target = []
                              | otherwise = (x : getStrsBefore xs target)

getStrsAfter :: [String] -> String -> [String]
getStrsAfter ([]) _ = []
getStrsAfter (x : xs) target  | x == target = xs
                              | otherwise = getStrsAfter xs target

separateArgs :: [String] -> String -> ([String], [String])
separateArgs args separator = (getStrsBefore args separator, getStrsAfter args separator)

run :: ([String], [String]) -> IO ()
run (filenames, args) = do
  files <- readFiles filenames
  defs <- parser files
  print defs
  pure()

build :: ([String], [String]) -> IO ()
build (filenames, name) =  do
  files <- readFiles filenames
  defs <- parser files
  pure()

launch :: ([String], [String]) -> IO ()
launch (binary, args) = pure()

argDispatch :: [String] -> IO ()
argDispatch ("-h" : args) = help args
argDispatch ("run" : args) = run $ separateArgs args "--"
argDispatch ("build" : args) = build $ separateArgs args "--"
argDispatch ("launch" : args) = launch $ separateArgs args "--"
argDispatch _ = help ["invalid"]

main :: IO ()
main = do
  args <- getArgs
  argDispatch args
  -- files <- readFiles args
  -- defs <- parser files
  -- print "ast: \n"
  -- print defs
  -- print "\n"
  -- (Binary env main_f) <- compile defs
  -- print "bin:\n"
  -- print env
  -- print "\n"
  -- print main_f
  -- print "\n"
  -- result <- exec env [] main_f [] []
  pure()

