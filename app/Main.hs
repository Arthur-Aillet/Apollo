module Main (main) where

import Ast.Ast
import Ast.CompileAST (Binary (..), generateBinary)
import Ast.Display (compile)
import Control.Monad.IO.Class
import Data.HashMap.Internal.Strict (keys)
import Data.List (isPrefixOf)
import Eval
import Eval.Exec
import Eval.Exec (Operator (Add, Or, Sub))
import Parser.List (argsToMaybeValues, hasNothing, removeMaybes)
import Parser.Parser (parser)
import Parser.Position (defaultPosition)
import Parser.Type (Parser (..), StackTrace)
import PreProcess
import System.Environment
import Prelude

defaultHelp :: String
defaultHelp =
  "\
  \Usage:\n\
  \\t./apollo [\ESC[33mrun\ESC[0m [files] (-- [args]) | \ESC[33mbuild\ESC[0m [files] (-- [name])| \ESC[33mlaunch\ESC[0m [binary] (-- [args])]\n\
  \use\n\
  \  ./apollo -h [\ESC[33mrun\ESC[0m | \ESC[33mbuild\ESC[0m | \ESC[33mlaunch\ESC[0m]\n\
  \for more details about these commands\n\
  \"

runHelp :: String
runHelp =
  "\
  \Compiles and execute the files with given args\n\
  \Usage:\n\
  \\t./apollo run \ESC[33m[files]\ESC[0m (-- \ESC[33m[args]\ESC[0m)\n\
  \\ESC[33mfiles\ESC[0m\t\tlist of files to execute\n\
  \\ESC[33margs\ESC[0m\t\targuments to execute with.\
  \if none are provided \"--\" is unnecessary\n\
  \"

buildHelp :: String
buildHelp =
  "\
  \Compiles the ginven files in a binary\n\
  \Usage:\n\
  \\t./apollo build \ESC[33m[files]\ESC[0m\ESC[0m (-- \ESC[33m[name]\ESC[0m)\n\
  \\ESC[33mfiles\ESC[0m\t\tlist of files to execute\n\
  \\ESC[33mname\ESC[0m\t\tthe name to give the binary.\
  \if none is given, it will be named a.out and \"--\" is unnecessary\n\
  \"

launchHelp :: String
launchHelp =
  "\
  \Executes the given binary with given args\n\
  \Usage:\n\
  \\t./apollo launch \ESC[33m[binary]\ESC[0m) (-- \ESC[33m[args]\ESC[0m)\n\
  \\ESC[33mbinary\ESC[0m\t\tbinary to execute\n\
  \\ESC[33margs\ESC[0m\t\targuments to execute with.\
  \if none are provided \"--\" is unnecessary\n\
  \"

invalidHelp :: String
invalidHelp =
  "\
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
getStrsBefore (x : []) target
  | x == target = []
  | otherwise = [x]
getStrsBefore (x : xs) target
  | x == target = []
  | otherwise = (x : getStrsBefore xs target)

getStrsAfter :: [String] -> String -> [String]
getStrsAfter ([]) _ = []
getStrsAfter (x : xs) target
  | x == target = xs
  | otherwise = getStrsAfter xs target

separateArgs :: [String] -> String -> ([String], [String])
separateArgs args separator = (getStrsBefore args separator, getStrsAfter args separator)

run :: ([String], [String]) -> IO ()
run (filenames, args) = do
  files <- readFiles filenames
  defs <- parser files
  (Binary env main_f) <- compile defs
  if hasNothing (argsToMaybeValues args) == False
    then do
      result <- exec (env, removeMaybes $ argsToMaybeValues args, main_f, [], [])
      case result of
        Left a -> putStrLn a
        Right a -> print a
      pure ()
    else do
      print "invalid args"
      pure ()

build :: ([String], [String]) -> IO ()
build (filenames, name) =
  if length (name) > 1
    then do
      putStr buildHelp
      pure ()
    else do
      files <- readFiles filenames
      defs <- parser files
      pure ()

launch :: ([String], [String]) -> IO ()
launch (binary, args) =
  if length (binary) > 1
    then do
      putStr launchHelp
      pure ()
    else do
      pure ()

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
