import AtomSpec (atomTests)
import Control.Monad
import ParserSpec (parserTests)
import Test.HUnit
import ParserSpec(parserTests)
import AtomSpec(atomTests)
import OperatorSpec(operatorTests)
import InstructionSpec(instructionTests)

main :: IO ()
main = Control.Monad.void (runTestTT tests)

tests :: Test
tests =
  TestList
    [
      parserTests,
      atomTests,
      operatorTests,
      instructionTests
    ]
