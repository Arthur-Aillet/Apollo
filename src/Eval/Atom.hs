--
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Atom
--

module Eval.Atom (Atom (..),
  bAtom, cAtom, iAtom, fAtom,
  atomCast
  ) where

-- import Text.Read

data Atom
  = AtomB Bool
  | AtomC Char Bool
  | AtomI Int
  | AtomF Float
  deriving (Show)

bAtom :: Atom -> Atom
bAtom (AtomB b) = AtomB b
bAtom x = AtomB $ case iAtom x of
  AtomI 0 -> False
  _ -> True

cAtom :: Atom -> Atom
cAtom (AtomB True) = cAtom $ AtomI 1
cAtom (AtomB False) = cAtom $ AtomI 0
cAtom (AtomC c s) = AtomC c s
cAtom (AtomI i) = AtomC (toEnum $ abs i) (i < 0)
cAtom (AtomF f) = cAtom $ iAtom $ AtomF f

iAtom :: Atom -> Atom
iAtom (AtomB True) = AtomI 1
iAtom (AtomB False) = AtomI 0
iAtom (AtomC c s) = AtomI $ (if s then negate else id) (fromEnum c)
iAtom (AtomI i) = AtomI i
iAtom (AtomF f) = AtomI $ fromEnum f

fAtom :: Atom -> Atom
fAtom (AtomF f) = AtomF f
fAtom x = case iAtom x of
  AtomI i -> AtomF $ toEnum i
  _ -> AtomF $ 0.0 / 0.0


atomCast :: (Atom -> Atom -> a) -> Atom -> Atom -> a
atomCast f (AtomF a) b = f (AtomF a) (fAtom b)
atomCast f a (AtomF b) = f (fAtom a) (AtomF b)
atomCast f a b = case a of
    AtomB _ -> case b of
      AtomB _ -> f a b
      AtomC _ _ -> f (cAtom a) b
      AtomI _ -> f (iAtom a) b
    AtomC _ _ -> case b of
      AtomB _ -> f a (cAtom b)
      AtomC _ _ -> f a b
      AtomI _ -> f a (cAtom b)
    AtomI _ -> case b of
      AtomB _ -> f a (iAtom b)
      AtomC _ _ -> f (cAtom a) b
      AtomI _ -> f a b

instance Num Atom where
  (+) (AtomB a) (AtomB b) = AtomB (a || b)
  (+) (AtomC a sa) (AtomC b sb) = if res <= 0
      then AtomC (toEnum (abs res)) True
      else AtomC (toEnum res) False
    where res = (if sa then negate else id) (fromEnum a)
                    +  (if sb then negate else id) (fromEnum b)
  -- (+) (AtomC a) (AtomC b) = AtomC $ toEnum $ fromEnum a + fromEnum b
  (+) (AtomI a) (AtomI b) = AtomI (a + b)
  (+) (AtomF a) (AtomF b) = AtomF (a + b)
  (+) a b = atomCast (+) a b


  (*) (AtomB a) (AtomB b) = AtomB (a && b)
  (*) (AtomC a sa) (AtomC b sb) = if res <= 0
      then AtomC (toEnum (abs res)) True
      else AtomC (toEnum res) False
    where res = (if sa then negate else id) (fromEnum a)
                    *  (if sb then negate else id) (fromEnum b)
  -- (*) (AtomC a) (AtomC b) = AtomC $ toEnum $ fromEnum a * fromEnum b
  (*) (AtomI a) (AtomI b) = AtomI (a * b)
  (*) (AtomF a) (AtomF b) = AtomF (a * b)
  (*) a b = atomCast (*) a b

  negate (AtomB x) = AtomI $ negate $ fromEnum x
  negate (AtomC c s) = AtomC c (not s)
  negate (AtomI i) = AtomI $ negate i
  negate (AtomF f) = AtomF $ negate f

  signum (AtomB _) = AtomI 1
  signum (AtomC _ s) = AtomC '\1' s
  signum (AtomI i) = AtomI $ signum i
  signum (AtomF f) = AtomF $ signum f

  abs (AtomB b) = AtomI $ fromEnum b
  abs (AtomC c _) = AtomC c False
  abs (AtomI i) = AtomI $ abs i
  abs (AtomF f) = AtomF $ abs f

  fromInteger x = AtomI $ fromEnum x

instance Eq Atom where
  (==) (AtomB a) (AtomB b) = a == b
  (==) (AtomC a sa) (AtomC b sb) = a == b && sa == sb
  -- (==) (AtomC a) (AtomC b) = a == b
  (==) (AtomI a) (AtomI b) = a == b
  (==) (AtomF a) (AtomF b) = a == b
  (==) a b = atomCast (==) a b

instance Ord Atom where
  (<=) (AtomB a) (AtomB b) = a <= b
  (<=) (AtomC a sa) (AtomC b sb) = sa < sb || (a <= b && not sa) || (b <= a && sa)
  -- (<=) (AtomC a) (AtomC b) = a <= b
  (<=) (AtomI a) (AtomI b) = a <= b
  (<=) (AtomF a) (AtomF b) = a <= b
  (<=) a b = atomCast (<=) a b

instance Fractional Atom where
  (/) (AtomB _) (AtomB _) = AtomB False
  (/) (AtomC a sa) (AtomC b sb) = fAtom (AtomC a sa) / fAtom (AtomC b sb)
  (/) (AtomI a) (AtomI b) = AtomI (a `div` b)
  (/) (AtomF a) (AtomF b) = AtomF (a / b)
  (/) a b = atomCast (/) a b

  fromRational x = AtomF $ fromRational x

readSAtomB :: ReadS Atom
readSAtomB ('#':'t':xs) = [(AtomB True, xs)]
readSAtomB ('#':'f':xs) = [(AtomB False, xs)]
readSAtomB _ = []

readSAtomC :: ReadS Atom
readSAtomC (c:xs) = [(AtomC c False, xs)]
readSAtomC _ = []

readSAtomI :: ReadS Atom
readSAtomI s = case reads s :: [(Int, String)] of
  (x, xs):_ -> [(AtomI x, xs)]
  [] -> [] :: [(Atom, String)]

readSAtomF :: ReadS Atom
readSAtomF s = case reads s :: [(Float, String)] of
  (x, xs):_ -> [(AtomF x, xs)]
  [] -> [] :: [(Atom, String)]

readSAtom :: ReadS Atom
readSAtom (' ':xs) = readSAtom xs
readSAtom ('\n':xs) = readSAtom xs
readSAtom ('\t':xs) = readSAtom xs
readSAtom string = [head $ readSAtomB string
                          ++ readSAtomI string
                          ++ readSAtomF string
                          ++ readSAtomC string]

instance Read Atom where
  readsPrec _ = readSAtom
