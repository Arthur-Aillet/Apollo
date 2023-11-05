{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Instructions
-}

module Eval.Syscall (Syscall (..), execSys) where

import Control.Exception (catch)
import Eval.Atom (Atom (..))
import Eval.Operator
  (
    Stack,
    Value (..),
  )
import System.IO (readFile')

data Syscall
  = Print
  | Read
  | Write
  | Append
  deriving (Show, Eq, Enum)

valueToString :: (Show a) => a -> Value -> Either String String
valueToString str val = case val of
  (VList []) -> Right ""
  (VList (VAtom (AtomC c _) : chars)) ->
    (c :) <$> valueToString str (VList chars)
  type' -> Left $ show str ++ " with non string but " ++ show type'

handler :: (Show a) => a -> IOError -> IO (Either String [Value])
handler str err = return $ Left $ show str ++ " Exception Caught: " ++ show err

toVChar :: Char -> Value
toVChar char = VAtom $ AtomC char True

execSys :: Stack -> Syscall -> IO (Either String Stack)
execSys (x : xs) Print = case x of
  (VList []) -> return (Right xs)
  (VList (VAtom (AtomC c _) : chars)) ->
    putStr [c] >> execSys (VList chars : xs) Print
  _ -> return (Left "Print with non string")
execSys [] Print = return $ Left "Print with empty stack"
execSys (x : xs) Read = case valueToString "Read" x of
  Left err -> return $ Left err
  Right name ->
    catch
      ( do
          content <- readFile' name
          return $ Right $ VList (map toVChar content) : xs
      )
      (handler "Read")
execSys [] Read = return $ Left "Read with empty stack"
execSys (x : y : xs) Write = case (valueToString m1 x, valueToString m2 y) of
  (Left err, _) -> return $ Left err
  (_, Left err) -> return $ Left err
  (Right name, Right content) ->
    catch (writeFile name content >> return (Right xs)) (handler "Write")
  where
    m1 = "Write file name"
    m2 = "Write file content"
execSys _ Write = return $ Left "Write without enough args on stack"
execSys (x : y : xs) Append = case (valueToString m1 x, valueToString m2 y) of
  (Left err, _) -> return $ Left err
  (_, Left err) -> return $ Left err
  (Right name, Right content) ->
    catch (appendFile name content >> return (Right xs)) (handler "Append")
  where
    m1 = "Append file name"
    m2 = "Append file content"
execSys _ Append = return $ Left "Append without enough args on stack"
