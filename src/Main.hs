module Main where

import Pipes         as P
import Pipes.Prelude as P

import Analysis
import Generator
import Parser
import Types

main :: IO ()
main = do
  temp <- obsCount parser
  Prelude.print temp
