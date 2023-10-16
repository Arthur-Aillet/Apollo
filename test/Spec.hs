import Control.Monad
import Test.HUnit
import ParserSpec(parserTests)

main :: IO ()
main = Control.Monad.void (runTestTT tests)

tests :: Test
tests =
  TestList
    [ parserTests
    ]
