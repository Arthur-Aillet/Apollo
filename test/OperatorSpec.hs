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
import Eval.Exec
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
    [ "operate +" ~: operate Add [AtomI 1, AtomI 2] ~?= Right (AtomI 3),
      "operate -" ~: operate Sub [AtomI 1, AtomI 2] ~?= Right (AtomI (-1)),
      "operate *" ~: operate Mul [AtomI 1, AtomI 2] ~?= Right (AtomI 2),
      "operate /" ~: operate Div [AtomI 1, AtomI 2] ~?= Right (AtomI 0),
      "operate ==" ~: operate Eq [AtomI 1, AtomI 2] ~?= Right (AtomB False),
      "operate <" ~: operate Lt [AtomI 1, AtomI 2] ~?= Right (AtomB True)
    ]

execOperatorTest :: Test
execOperatorTest =
  TestList
    [ "execOperator 2 in stack" ~: execOperator [VAtom (AtomI 1), VAtom (AtomI 2)] Add ~?= Right [VAtom (AtomI 3)],
      "execOperator 1 in stack" ~: execOperator [VAtom (AtomI 1)] Add ~?= Left "not enough arguments in the stack",
      "execOperator 0 in stack" ~: execOperator [] Add ~?= Left "not enough arguments in the stack",
      "execOperator inf in stack" ~: head <$> execOperator (repeat (VAtom (AtomI 1))) Add ~?= Right (VAtom (AtomI 2))
    ]
