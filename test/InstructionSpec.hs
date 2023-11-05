{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- InstructionSpec
-}

module InstructionSpec
  ( instructionTests,
  )
where

import Test.HUnit

instructionTests :: Test
instructionTests =
  TestList
    []

{--
moveForwardTests :: Test
moveForwardTests =
  TestList
    [ "moveForward 0" ~: moveForward 0 insts ~?= Right $ fst $insts,
      "moveForward 1" ~: moveForward 1 insts ~?= Right (tail $ fst insts),
      "moveForward 1 on empty" ~: isLeft (moveForward 1 []) ~?= True,
      "moveForward too many" ~: isLeft (moveForward 30 insts) ~?= True,
      "moveForward -1" ~: isLeft (moveForward (-1) insts) ~?= True
    ]
  where
    (_, insts) = createEnv !! 0
--}
