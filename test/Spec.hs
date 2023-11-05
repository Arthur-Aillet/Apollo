import AtomSpec (atomTests)
import Control.Monad
import InstructionSpec (instructionTests)
import OperatorSpec (operatorTests)
import Parser.ParserSpec (parserTests)
import Test.HUnit

main :: IO ()
main = Control.Monad.void (runTestTT tests)

tests :: Test
tests =
  TestList
    [ parserTests,
      atomTests,
      operatorTests,
      instructionTests
    ]
