{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Main
-}

module Main (main) where

import Ast.CompileAST (Binary (..))
import Ast.Display (compile)
import Control.Monad (void)
import Eval (exec)
import Eval.Instructions (Env, Insts)
import Parser.List (argsToMaybeValues, hasNothing, removeMaybes)
import Parser.Parser (parser)
import PreProcess (readFiles)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import Eval.Exec (Atom (AtomC), Value (VAtom, VList))

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

helpMsg :: [String] -> IO ()
helpMsg [] = putStr defaultHelp
helpMsg ["run"] = putStr runHelp
helpMsg ["build"] = putStr buildHelp
helpMsg ["launch"] = putStr launchHelp
helpMsg _ = putStr invalidHelp

getStrsBefore :: [String] -> String -> [String]
getStrsBefore [] _ = []
getStrsBefore [x] target
  | x == target = []
  | otherwise = [x]
getStrsBefore (x : xs) target
  | x == target = []
  | otherwise = x : getStrsBefore xs target

getStrsAfter :: [String] -> String -> [String]
getStrsAfter [] _ = []
getStrsAfter (x : xs) target
  | x == target = xs
  | otherwise = getStrsAfter xs target

separateArgs :: [String] -> String -> ([String], [String])
separateArgs args separator =
  (getStrsBefore args separator, getStrsAfter args separator)

execute :: Env -> [String] -> Insts -> IO ()
execute env args main_f = do
  result <- exec (env, removeMaybes $ argsToMaybeValues args, main_f, [], [])
  case result of
    Left a -> putStrLn a
    Right a -> print a

charToAtomC :: Char -> Value
charToAtomC c = VAtom $ AtomC c False

stringToVal :: String -> Value
stringToVal str = VList $ map charToAtomC str

stringsToVals :: [String] -> [Value]
stringsToVals = map stringToVal

run :: ([String], [String]) -> IO ()
run (filenames, args) = do
  files <- readFiles filenames
  defs <- parser files
  (Binary env main_f) <- compile defs
  if not (hasNothing (argsToMaybeValues args))
    then execute env args main_f
    else void $ print "invalid args"

build :: ([String], [String]) -> IO ()
build (filenames, name)
  | length name > 1 = void (putStr buildHelp)
  | otherwise = do
      files <- readFiles filenames
      defs <- parser files
      pure ()

launch :: ([String], [String]) -> IO ()
launch (binary, _)
  | length binary > 1 = putStr launchHelp >> pure ()
  | otherwise = pure ()

argDispatch :: [String] -> IO ()
argDispatch ("-h" : args) = helpMsg args >> exitSuccess
argDispatch ("run" : args) = run $ separateArgs args "--"
argDispatch ("build" : args) = build $ separateArgs args "--"
argDispatch ("launch" : args) = launch $ separateArgs args "--"
argDispatch _ = helpMsg ["invalid"] >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  argDispatch args
