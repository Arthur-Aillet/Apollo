{-
-- EPITECH PROJECT, 2023
-- PreProcess
-- File description:
-- Parser type
-}

module PreProcess (readFiles) where

readFiles :: [String] -> IO String
readFiles [] = return []
readFiles (x : xs) = do
  file <- readFile x
  other <- readFiles xs
  return $ file ++ other
