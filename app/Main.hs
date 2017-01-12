module Main where

import System.Environment
import Text.Parsec (parse)

import Parser
import Interpreter
import Repl

main :: IO ()
main = runRepl
