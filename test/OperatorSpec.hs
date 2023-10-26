--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- OperatorSpec
--

module OperatorSpec
  ( operatorTests,
  )
where

import Eval.Atom
import Eval.Operator
import Test.HUnit

operatorTests :: Test
operatorTests =
  TestList
    [ operateTest,
      execOperatorTest
    ]

operateTest :: Test
operateTest =
  TestList
    [ "operate +" ~: operate Addition [AtomI 1, AtomI 2] ~?= Right (AtomI 3),
      "operate -" ~: operate Subtraction [AtomI 1, AtomI 2] ~?= Right (AtomI (-1)),
      "operate *" ~: operate Multiplication [AtomI 1, AtomI 2] ~?= Right (AtomI 2),
      "operate /" ~: operate Division [AtomI 1, AtomI 2] ~?= Right (AtomI 0),
      "operate ==" ~: operate Eq [AtomI 1, AtomI 2] ~?= Right (AtomB False),
      "operate <" ~: operate Less [AtomI 1, AtomI 2] ~?= Right (AtomB True)
    ]

execOperatorTest :: Test
execOperatorTest =
  TestList
    [ "execOperator 2 in stack" ~: execOperator [AtomI 1, AtomI 2] Addition ~?= Right [AtomI 3],
      "execOperator 1 in stack" ~: execOperator [AtomI 1] Addition ~?= Left "not enough arguments in the stack",
      "execOperator 0 in stack" ~: execOperator [] Addition ~?= Left "not enough arguments in the stack",
      "execOperator inf in stack" ~: head <$> execOperator (repeat (AtomI 1)) Addition ~?= Right (AtomI 2)
    ]
