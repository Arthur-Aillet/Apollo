{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- Bytecode
-}

module Ast.Bytecode
  ( fromBytecode,
    toBytecode,
    encode,
    decode,
    Bytes,
  )
where

import Ast.Type (Type (..))
import Data.Word
import Eval.Atom (Atom (..), bAtom, cAtom, iAtom)
import Eval.Instructions (Env, Index, Instruction (..))
import Eval.Operator (Operator (..))
import Eval.Syscall (Syscall (..))
import Unsafe.Coerce

type Bytes = Word64

type Bytecode = (Bytes, Bytes, [Bytes])

data InstructionEnum
  = EPushD
  | EStore
  | ETake
  | EAssign
  | EArrAssign
  | EPushI
  | ECallD
  | ECallI
  | ECast
  | ECallS
  | EOp
  | ESys
  | EJumpIfFalse
  | EJump
  | ERet
  | EInstructionError -- out of bounds
  deriving (Show, Eq, Enum, Ord)

-- insure this is synced to the types in Eval.Instructions

decode :: [Bytes] -> Either String Env
decode [] = Right []
decode (i : s : xs) = case fromBytecode (take size xs) of
  Left err -> Left err
  Right y -> case decode (drop size xs) of
    Left err -> Left err
    Right ys -> Right ((idx, y) : ys)
  where
    size = bytecodeInt s
    idx = bytecodeIndex i
decode [_] = Left "invalid bytecode (almost empty decode)"

encode :: Env -> [Bytes]
encode ((idx, func) : xs) =
  (indexBytecode idx : intBytecode (length bytes) : bytes)
    ++ encode xs
  where
    bytes = toBytecode func
encode [] = []

atomBytecode :: Atom -> [Bytes]
atomBytecode (AtomB x) = [typeBytecode TypeBool, toEnum $ fromEnum x]
atomBytecode (AtomC x y) =
  [typeBytecode TypeChar, atomBytecode (iAtom (AtomC x y)) !! 1]
atomBytecode (AtomI x) = [typeBytecode TypeInt, intBytecode x]
atomBytecode (AtomF x) = [typeBytecode TypeFloat, unsafeCoerce x]

intBytecode :: Int -> Bytes
intBytecode = unsafeCoerce

indexBytecode :: Index -> Bytes
indexBytecode = toEnum

instructionBytecode :: InstructionEnum -> Bytes
instructionBytecode x = intBytecode $ fromEnum x

typeBytecode :: Type -> Bytes
typeBytecode TypeBool = 0
typeBytecode TypeChar = 1
typeBytecode TypeInt = 2
typeBytecode TypeFloat = 3
typeBytecode (TypeList _) = 4

operatorBytecode :: Operator -> Bytes
operatorBytecode x = toEnum $ fromEnum x

syscallBytecode :: Syscall -> Bytes
syscallBytecode x = toEnum $ fromEnum x

toBytecode :: [Instruction] -> [Bytes]
toBytecode (instr : xs) = (a : b : c) ++ toBytecode xs
  where
    (a, b, c) = toBytecode' instr
toBytecode [] = []

toBytecode' :: Instruction -> Bytecode
toBytecode' (PushD x) =
  (instructionBytecode EPushD, intBytecode 2, atomBytecode x)
toBytecode' Store = (instructionBytecode EStore, 0, [])
toBytecode' (Take x) = (instructionBytecode ETake, 1, [indexBytecode x])
toBytecode' (Assign x) = (instructionBytecode EAssign, 1, [indexBytecode x])
toBytecode' (ArrAssign x) =
  (instructionBytecode EArrAssign, 1, [indexBytecode x])
toBytecode' (PushI x) = (instructionBytecode EPushI, 1, [indexBytecode x])
toBytecode' (CallD x) = (instructionBytecode ECallD, 1, [indexBytecode x])
toBytecode' (CallI x) = (instructionBytecode ECallI, 1, [indexBytecode x])
toBytecode' (Cast x) = (instructionBytecode ECast, 1, [typeBytecode x])
toBytecode' CallS = (instructionBytecode ECallS, 0, [])
toBytecode' (Op x) = (instructionBytecode EOp, 1, [operatorBytecode x])
toBytecode' (Sys x) = (instructionBytecode ESys, 1, [syscallBytecode x])
toBytecode' (JumpIfFalse x) =
  (instructionBytecode EJumpIfFalse, 1, [intBytecode x])
toBytecode' (Jump x) = (instructionBytecode EJump, 1, [intBytecode x])
toBytecode' Ret = (instructionBytecode ERet, 0, [])

bytecodeAtom :: Bytes -> Bytes -> Atom
bytecodeAtom t x
  | t == typeBytecode TypeBool = bAtom (bytecodeAtom (typeBytecode TypeInt) x)
  | t == typeBytecode TypeChar = cAtom (bytecodeAtom (typeBytecode TypeInt) x)
  | t == typeBytecode TypeInt = AtomI (bytecodeInt x)
  | t == typeBytecode TypeFloat = AtomF (unsafeCoerce x)
  | otherwise = AtomB False

bytecodeInt :: Bytes -> Int
bytecodeInt = unsafeCoerce

bytecodeIndex :: Bytes -> Index
bytecodeIndex x = toEnum $ fromEnum x

bytecodeInstruction :: Bytes -> Either String InstructionEnum
bytecodeInstruction x =
  if fromEnum x < fromEnum EInstructionError
    then Right (toEnum $ bytecodeInt x)
    else Left ("Instruction bytecode deos not exist: " ++ show x)

bytecodeType :: Bytes -> Type
bytecodeType 0 = TypeBool
bytecodeType 1 = TypeChar
bytecodeType 2 = TypeInt
bytecodeType 3 = TypeFloat
bytecodeType 4 = TypeList Nothing
bytecodeType _ = TypeList Nothing

bytecodeOperator :: Bytes -> Operator
bytecodeOperator x = toEnum $ fromEnum x

bytecodeSyscall :: Bytes -> Syscall
bytecodeSyscall x = toEnum $ fromEnum x

fromBytecode :: [Bytes] -> Either String [Instruction]
fromBytecode (instr : count : xs) = case bytecodeInstruction instr of
  Left err -> Left err
  Right einstr -> case fromBytecode' einstr (bytecodeInt count) xs of
    Left err -> Left err
    Right (bytes, rest) -> (bytes :) <$> fromBytecode rest
fromBytecode [] = Right []
-- fromBytecode [0] = Right []
fromBytecode [x] =
  Left $ "invalid bytecode (almost empty fromBytecode):" ++ show x

fromBytecode' :: InstructionEnum -> Int -> [Bytes] -> Either String (Instruction, [Bytes])
fromBytecode' EPushD 2 (x : y : xs) = Right (PushD (bytecodeAtom x y), xs)
fromBytecode' EStore 0 xs = Right (Store, xs)
fromBytecode' ETake 1 (x : xs) = Right (Take (bytecodeInt x), xs)
fromBytecode' EAssign 1 (x : xs) = Right (Assign (bytecodeIndex x), xs)
fromBytecode' EArrAssign 1 (x : xs) = Right (ArrAssign (bytecodeIndex x), xs)
fromBytecode' EPushI 1 (x : xs) = Right (PushI (bytecodeIndex x), xs)
fromBytecode' ECallD 1 (x : xs) = Right (CallD (bytecodeIndex x), xs)
fromBytecode' ECallI 1 (x : xs) = Right (CallI (bytecodeIndex x), xs)
fromBytecode' ECast 1 (x : xs) = Right (Cast (bytecodeType x), xs)
fromBytecode' ECallS 0 xs = Right (CallS, xs)
fromBytecode' EOp 1 (x : xs) = Right (Op (bytecodeOperator x), xs)
fromBytecode' ESys 1 (x : xs) = Right (Sys (bytecodeSyscall x), xs)
fromBytecode' EJumpIfFalse 1 (x : xs) = Right (JumpIfFalse (bytecodeInt x), xs)
fromBytecode' EJump 1 (x : xs) = Right (Jump (bytecodeInt x), xs)
fromBytecode' ERet 0 xs = Right (Ret, xs)
fromBytecode' i count xs =
  Left ("impossible " ++ show i ++ " " ++ show count ++ " " ++ show xs)
