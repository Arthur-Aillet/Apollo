module Main (main) where

-- import Data.Either (Either (Right))
-- import Eval
import Parser.StackTrace (StackTrace (..))
import Parser.Position (Position (..), defaultPosition)
import System.IO (BufferMode (..), hGetContents', hIsTerminalDevice, hSetBuffering, stdin, stdout)
import Prelude
import Parser.Type (Parser(..))
import System.Console.Haskeline
    ( getInputLine,
      completeWord,
      simpleCompletion,
      runInputT,
      Completion,
      InputT,
      Settings(Settings, autoAddHistory, complete, historyFile) )
import Control.Monad.IO.Class

import Data.HashMap.Internal.Strict (keys)
import Data.List (isPrefixOf)
-- import Parser.String (parseStringWithHandleBackslash)
import Parser.Condition(parseOperable, parseApredicat, parseCondOperation)

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

getresult :: Either StackTrace (Int , String, Position)
getresult = Right (12, "12", Position 12 12)

getInstructions :: IO ()
getInstructions = do
  new_line <- runInputT newSettings haskelineGetline
  case runParser parseCondOperation new_line defaultPosition of
    Right (i, str, pos) -> do
      print(new_line)
      print(i)
      print(str)
      getInstructions
    Left a -> do
      print a

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  bool <- hIsTerminalDevice stdin
  if bool
    then getInstructions
    else do
      content <- hGetContents' stdin
      print(content)
