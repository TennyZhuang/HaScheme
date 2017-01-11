module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ head args
