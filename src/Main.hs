module Main where

import Pipes            as P
import Pipes.Prelude    as P
import Pipes.ByteString as PB

import Analysis
import Generator
import Parser
import Types

main :: IO ()
main = runEffect $ perfectInput >-> P.take 10000 >-> PB.stdout
{-
  temp <- obsCount parser
  Prelude.print temp
-}
