import Control.Monad
import Test.HUnit
import ParserSpec(parserTests)
import AtomSpec(atomTests)

main :: IO ()
main = Control.Monad.void (runTestTT tests)

tests :: Test
tests =
  TestList
    [
      parserTests,
      atomTests
    ]
