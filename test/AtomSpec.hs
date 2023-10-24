--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- AtomSpec
--

module AtomSpec (
  atomTests
  -- bAtomTests, iAtomTests, cAtomTests, fAtomTests
  ) where

import Test.HUnit
import Atom.Atom (Atom (..), bAtom, cAtom, iAtom, fAtom, atomCast)
import Text.Read (readMaybe)


atomTests :: Test
atomTests =
  TestList
    [
      bAtomTests,
      iAtomTests,
      cAtomTests,
      fAtomTests,
      atomEqTests,
      atomOrdTests,
      atomCastTests,
      atomFracTests,
      atomAddTests,
      atomMulTests,
      atomNumTests,
      readAtomTests
    ]

bAtomTests :: Test
bAtomTests =
  TestList
    [
      "bAtom from Bool" ~: bAtom (AtomB True) ~?= AtomB True,
      "bAtom from Char" ~: bAtom (AtomC 'C' False) ~?= AtomB True,
      "bAtom from Int" ~: bAtom (AtomI 465) ~?= AtomB True,
      "bAtom from 0" ~: bAtom (AtomI 0) ~?= AtomB False,
      "bAtom from neg Int" ~: bAtom (AtomI $ -465) ~?= AtomB True,
      "bAtom from Float" ~: bAtom (AtomF 46.4) ~?= AtomB True
    ]

iAtomTests :: Test
iAtomTests =
  TestList
    [
      "iAtom from True" ~: iAtom (AtomB True) ~?= AtomI 1,
      "iAtom from False" ~: iAtom (AtomB False) ~?= AtomI 0,
      "iAtom from Char" ~: iAtom (AtomC 'C' False) ~?= AtomI (fromEnum 'C'),
      "iAtom from neg Char" ~: iAtom (AtomC 'C' True) ~?= AtomI (negate $ fromEnum 'C'),
      "iAtom from Int" ~: iAtom (AtomI 465) ~?= AtomI 465,
      "iAtom from Float" ~: iAtom (AtomF 46.4) ~?= AtomI 46
    ]

cAtomTests :: Test
cAtomTests =
  TestList
    [
      "cAtom from True" ~: cAtom (AtomB True) ~?= AtomC '\1' False,
      "cAtom from False" ~: cAtom (AtomB False) ~?= AtomC '\0' False,
      "cAtom from Char" ~: cAtom (AtomC 'C' False) ~?= AtomC 'C' False,
      "cAtom from neg Char" ~: cAtom (AtomC 'C' True) ~?= AtomC 'C' True,
      "cAtom from Int" ~: cAtom (AtomI 465) ~?= AtomC '\465' False,
      "cAtom from Float" ~: cAtom (AtomF 46.4) ~?= AtomC '\46' False
    ]

fAtomTests :: Test
fAtomTests =
  TestList
    [
      "fAtom from True" ~: fAtom (AtomB True) ~?= AtomF 1.0,
      "fAtom from False" ~: fAtom (AtomB False) ~?= AtomF 0.0,
      "fAtom from Char" ~: fAtom (AtomC 'C' False) ~?= AtomF 67.0,
      "fAtom from neg Char" ~: fAtom (AtomC 'C' True) ~?= AtomF (-67.0),
      "fAtom from Int" ~: fAtom (AtomI 465) ~?= AtomF 465.0,
      "fAtom from Float" ~: fAtom (AtomF 46.4) ~?= AtomF 46.4
    ]

atomCastTests :: Test
atomCastTests =
  TestList
    [
      "Cast Char::Char" ~: (atomCast const (cAtom a) (cAtom b), atomCast seq (cAtom a) (cAtom b)) ~?= (cAtom a, cAtom b),
      "Cast Char::Int" ~: (atomCast const (cAtom a) (iAtom b), atomCast seq (cAtom a) (iAtom b)) ~?= (cAtom a, iAtom b),
      "Cast Char::Float" ~: (atomCast const (cAtom a) (fAtom b), atomCast seq (cAtom a) (fAtom b)) ~?= (cAtom a, fAtom b),
      "Cast Char::Bool" ~: (atomCast const (cAtom a) (bAtom b), atomCast seq (cAtom a) (bAtom b)) ~?= (cAtom a, bAtom b),

      "Cast Int::Char" ~: (atomCast const (iAtom a) (cAtom b), atomCast seq (iAtom a) (cAtom b)) ~?= (iAtom a, cAtom b),
      "Cast Int::Int" ~: (atomCast const (iAtom a) (iAtom b), atomCast seq (iAtom a) (iAtom b)) ~?= (iAtom a, iAtom b),
      "Cast Int::Float" ~: (atomCast const (iAtom a) (fAtom b), atomCast seq (iAtom a) (fAtom b)) ~?= (iAtom a, fAtom b),
      "Cast Int::Bool" ~: (atomCast const (iAtom a) (bAtom b), atomCast seq (iAtom a) (bAtom b)) ~?= (iAtom a, bAtom b),

      "Cast Float::Char" ~: (atomCast const (fAtom a) (cAtom b), atomCast seq (fAtom a) (cAtom b)) ~?= (fAtom a, cAtom b),
      "Cast Float::Int" ~: (atomCast const (fAtom a) (iAtom b), atomCast seq (fAtom a) (iAtom b)) ~?= (fAtom a, iAtom b),
      "Cast Float::Float" ~: (atomCast const (fAtom a) (fAtom b), atomCast seq (fAtom a) (fAtom b)) ~?= (fAtom a, fAtom b),
      "Cast Float::Bool" ~: (atomCast const (fAtom a) (bAtom b), atomCast seq (fAtom a) (bAtom b)) ~?= (fAtom a, bAtom b),

      "Cast Bool::Char" ~: (atomCast const (bAtom a) (cAtom b), atomCast seq (bAtom a) (cAtom b)) ~?= (bAtom a, cAtom b),
      "Cast Bool::Int" ~: (atomCast const (bAtom a) (iAtom b), atomCast seq (bAtom a) (iAtom b)) ~?= (bAtom a, iAtom b),
      "Cast Bool::Float" ~: (atomCast const (bAtom a) (fAtom b), atomCast seq (bAtom a) (fAtom b)) ~?= (bAtom a, fAtom b),
      "Cast Bool::Bool" ~: (atomCast const (bAtom a) (bAtom b), atomCast seq (bAtom a) (bAtom b)) ~?= (bAtom a, bAtom b)
    ]
  where
    a = AtomC 'a' False
    b = AtomC 'b' False

atomEqTests :: Test
atomEqTests =
  TestList
    [
      "Char == Int" ~: (AtomC 'A' False == AtomI 65) ~?= True,
      "Int == Int" ~: AtomI 65 == AtomI 65 ~?= True,
      "Bool == Int" ~: AtomB False == AtomI 0 ~?= True,
      "Float == Int" ~: AtomF 65.0 == AtomI 65 ~?= True
    ]

atomOrdTests :: Test
atomOrdTests =
  TestList
    [
      "Char <= Int" ~: (AtomC 'A' False <= AtomI 65) ~?= True,
      "Char <= Char" ~: (AtomC 'a' True <= AtomC 'A' True) ~?= True,
      "Int < Int" ~: AtomI (-8) < AtomI (-6) ~?= True,
      "Bool > Bool" ~: AtomB True > AtomB False ~?= True,
      "Float => Int" ~: 65.4 >= AtomI 65 ~?= True
    ]

atomFracTests :: Test
atomFracTests =
  TestList
    [
      "Int / Int" ~: AtomI 465 / AtomI 23 ~?= AtomI (465 `div` 23),
      "Char / Int" ~: (AtomC 'A' False / AtomI 65) ~?= cAtom (AtomI (97 `div` 65)),
      "Char / Char" ~: (AtomC 'a' True / AtomC 'A' True) ~?= AtomF ((-97.0) / (-65.0)),
      "Bool / Bool" ~: AtomB True / AtomB False ~?= AtomB False,
      "Float / Int" ~: AtomF 65.4 / AtomI 65 ~?= AtomF (65.4 / 65.0)
    ]

atomAddTests :: Test
atomAddTests =
  TestList
    [
      "Int + Int" ~: AtomI 645 + AtomI 758 ~?= AtomI (645 + 758),
      "Char + neg Char" ~: AtomC 'A' False + AtomC 'C' True ~?= cAtom (AtomI (65 - 67)),
      "neg Char + Char" ~: AtomC 'A' True + AtomC 'C' False ~?= cAtom (AtomI (67 - 65)),
      "Float + Float" ~: AtomF 6.45 + AtomF 75.8 ~?= AtomF (6.45 + 75.8),
      "Bool + Bool" ~: AtomB False + AtomB True ~?= AtomB True,
      "Float + Bool" ~: AtomF 6.45 + AtomB True ~?= AtomF 7.45
    ]

atomMulTests :: Test
atomMulTests =
  TestList
    [
      "Int * Int" ~: AtomI 645 * AtomI 758 ~?= AtomI (645 * 758),
      "Char * neg Char" ~: AtomC 'A' False * AtomC 'C' True ~?= cAtom (AtomI (65 * (-67))),
      "Char * Char" ~: AtomC 'A' False * AtomC 'C' False ~?= cAtom (AtomI (67 * 65)),
      "neg Char * neg Char" ~: AtomC 'A' True * AtomC 'C' True ~?= cAtom (AtomI ((-67) * (-65))),
      "Float * Float" ~: AtomF 6.45 * AtomF 75.8 ~?= AtomF (6.45 * 75.8),
      "Bool * Bool" ~: AtomB True * AtomB True ~?= AtomB True,
      "Float * Bool" ~: AtomF 6.45 * AtomB True ~?= AtomF 6.45
    ]

atomNumTests :: Test
atomNumTests =
  TestList
    [
      "Int - Int" ~: AtomI 645 - AtomI 758 ~?= AtomI (645 - 758),
      "Char - Char" ~: cAtom (AtomI 645) - cAtom (AtomI 758) ~?= cAtom (AtomI (645 - 758)),
      "signum Float" ~: signum (AtomF 6.45) ~?= signum 6.45,
      "abs Bool" ~: abs (AtomB True) ~?= AtomB True,
      "fromInteger" ~: (fromInteger 58 :: Atom) ~?= AtomI 58
    ]


readAtomTests :: Test
readAtomTests =
  TestList
    [
      "Read [#t, #f, 0, 1, -1, 0.0, -0.5, a, A]"
        ~: (readMaybe "[#t, #f, 0, 1, -1, 0.0, -0.5, a, A]" :: Maybe [Atom])
          ~?= Just [AtomB True, AtomB False, AtomI 0, AtomI 1, AtomI (-1), AtomF 0.0, AtomF (- 0.5), AtomC 'a' False, AtomC 'A' False]
    ]
