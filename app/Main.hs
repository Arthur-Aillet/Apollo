module Main (main) where

import Ast.Display (compile)
import Data.List (delete, elem, elemIndex)
import Eval
import Parser.Parser (parser)
import PreProcess (readFiles)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString as ByteString
import qualified Data.Binary as Binary
import Ast.Bytecode
import Control.Monad (void)

defaultHelp :: String
defaultHelp =
  "\
  \Usage:\n\
  \\t./apollo [\
  \ \ESC[33mrun\ESC[0m [files] (-- [args]) |\
  \ \ESC[33mbuild\ESC[0m [files] (-- [name]) |\
  \ \ESC[33mlaunch\ESC[0m [binary] (-- [args]) |\
  \ \ESC[33mcompiled\ESC[0m [files] ]\n\
  \use\n\
  \  ./apollo -h [ \
  \ \ESC[33mrun\ESC[0m |\
  \ \ESC[33mbuild\ESC[0m |\
  \ \ESC[33mlaunch\ESC[0m |\
  \ \ESC[33mcompiled\ESC[0m]\n\
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
  \\t./apollo build \ESC[33m[files]\ESC[0m\ESC[0m (-o \ESC[33m[name]\ESC[0m)\n\
  \\ESC[33mfiles\ESC[0m\t\tlist of files to execute\n\
  \\ESC[33mname\ESC[0m\t\tthe name to give the binary.\
  \if none is given, it will be named a.out and \"-o\" is unnecessary\n\
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

compiledHelp :: String
compiledHelp = "pending"

invalidHelp :: String
invalidHelp =
  "\
  \Invalid -h arguments\n\
  \use\n\
  \  \ESC[33m./apollo -h\ESC[0m\n\
  \for help\n\
  \"

getIndex :: [String] -> String -> Int
getIndex list element = case elemIndex element list of
  Just a -> a
  Nothing -> -1

isAfter :: [String] -> String -> String -> Bool
isAfter list a b = getIndex list b - getIndex list a == 1

getName :: [String] -> String
getName (x : xs)
  | isAfter xs "-o" x = x
  | otherwise = getName xs
getName [] = ""

extractname :: [String] -> ([String], String)
extractname strs =
  if "-o" `elem` strs
    then (delete "-o" (delete (getName strs) strs), getName strs)
    else (strs, "a.out")

help :: [String] -> IO ()
help [] = putStr defaultHelp
help list
  | isAfter list "-h" "run" = putStr runHelp
  | isAfter list "-h" "build" = putStr buildHelp
  | isAfter list "-h" "launch" = putStr launchHelp
  | isAfter list "-h" "compile" = putStr compiledHelp
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

charToAtomC :: Char -> Value
charToAtomC c = VAtom $ AtomC c False

stringToVal :: String -> Value
stringToVal str = VList $ map charToAtomC str

stringsToVals :: [String] -> [Value]
stringsToVals = map stringToVal

execute :: Env -> [String] -> Insts -> IO Int
execute env args main_f = do
  result <- exec (env, [VList $ stringsToVals args], main_f, [], [])
  case result of
    Left a -> do
      putStrLn a
      exitWith (ExitFailure 0)
    Right (Just (VAtom (AtomI a))) -> exitWith (ExitFailure a)
    Right (Just (VAtom (AtomC a _))) -> exitWith (ExitFailure $ fromEnum a)
    Right (Just (VAtom (AtomF a))) -> exitWith (ExitFailure (round a :: Int))
    Right _ -> exitWith (ExitFailure 1)

run :: ([String], [String]) -> IO Int
run (filenames, args) = do
  files <- readFiles filenames
  defs <- parser files
  env <- compile defs
  execute env args (snd $ head env)

build :: ([String], [String]) -> IO Int
build (filenames, name)
  | length name > 1 = putStr buildHelp >> return 1
  | otherwise = do
      files <- readFiles filenames
      defs <- parser files
      env <- compile defs
      let encoded = encode env
      let file = head (name ++ ["a.bin"])
      ByteString.writeFile file (ByteString.toStrict $ Binary.encode encoded)
      return 0

launch :: ([String], [String]) -> IO Int
launch (binary, args)
  | length binary > 1 = putStr launchHelp >> return 1
  | otherwise = do
      bytestring <- ByteString.readFile (head binary)
      let bytes = Binary.decode (ByteString.fromStrict bytestring) :: [Bytes]
      let env = decode bytes
      case env of
        Right env2 -> execute env2 args (snd $ head env2) >> return 0
        Left err -> putStrLn err >> return 1


argDispatch :: [String] -> IO Int
argDispatch list | "-h" `elem` list = do
  help list
  exitWith (ExitFailure 0)
argDispatch ("run" : args) = run $ separateArgs args "--"
argDispatch ("build" : args) = build $ separateArgs args "--"
argDispatch ("launch" : args) = launch $ separateArgs args "--"
argDispatch _ = do
  help ["invalid"]
  exitWith (ExitFailure 0)

main :: IO Int
main = do
  args <- getArgs
  argDispatch args
