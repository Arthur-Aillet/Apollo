{-
-- EPITECH PROJECT, 2023
-- apollo
-- File description:
-- ASM
-}

module Eval.ASM (disassemble) where

import Eval.Instructions

disassemble :: Env -> [[String]]
disassemble = map (\(i, func) -> ("@" ++ show i ++ ":") : disassembleI func)

disassembleI :: [Instruction] -> [String]
disassembleI = map (\x ->  "  "  ++ show x)
