--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- InstructionSpec
--

module InstructionSpec (
    instructionTests
    ) where

import Test.HUnit
import Eval.Instructions
import Eval.Exec
import Data.Maybe
import Data.HashMap.Lazy ((!?))
import Data.Either

instructionTests :: Test
instructionTests =
    TestList
        [
            moveForwardTests
        ]

moveForwardTests :: Test
moveForwardTests =
    TestList
        [
            "moveForward 0" ~: moveForward 0 insts ~?= Right insts,
            "moveForward 1" ~: moveForward 1 insts ~?= Right (tail insts),
            "moveForward 1 on empty" ~: isLeft (moveForward 1 []) ~?= True,
            "moveForward too many" ~: isLeft (moveForward 30 insts) ~?= True,
            "moveForward -1" ~: isLeft (moveForward (-1) insts) ~?= True
        ]
    where (_, insts) = fromJust $ createEnv !? "abs"
