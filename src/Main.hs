module Main where

import Options.Applicative
import Pipes               as P
import Pipes.Prelude       as P
import Pipes.ByteString    as PB (stdout)

import Analysis
import Generator
import Parser
import Types

main :: IO ()
main = do
  opts <- execParser (options `withInfo` "Weather balloon megaprocessor")
  Prelude.print opts

  -- runEffect $ perfectInput >-> P.take 500000000 >-> PB.stdout
  -- abc <- P.last (perfectInput >-> P.take 500000000)
  -- Prelude.print abc
{-
  temp <- obsCount parser
  Prelude.print temp
-}

data Options
  = Generate Integer GenOpts
  | Process  ProcOpts
  deriving (Show)

data GenOpts
  = Perfect
  | Realistic
  | Nightmare
  | Custom Parameters
  deriving (Show)

data ProcOpts
  = MinTemp
  | MaxTemp
  | MeanTemp
  | StationDistrib
  | Distance Integer
  | Normalise DistUnit TempUnit
  deriving (Show)

-- XXX Should carry this at type level using definitions in Types
data DistUnit = KM | MI | ME deriving (Show)
data TempUnit = C | K | F    deriving (Show)

-- With thanks to ThoughtBot, a free --help field
-- https://robots.thoughtbot.com/applicative-options-parsing-in-haskell
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

options :: Parser Options
options = subparser $
  command "generate" (parseGen  `withInfo` "Generate sample data") <>
  command "process"  (parseProc `withInfo` "Parse and process data")

parseGen :: Parser Options
parseGen = Generate
  <$> argument auto (metavar "NUM_LINES")
  <*> parseGenOpts

parseGenOpts :: Parser GenOpts
parseGenOpts = subparser $
  command "perfect"   (pure Perfect
    `withInfo` "Perfect clean, ordered data") <>
  command "realistic" (pure Realistic
    `withInfo` "Data with some reordering and mutation applied") <>
  command "nightmare" (pure Nightmare
    `withInfo` "Probably-unparseable data") <>
  command "custom"    (Custom <$> parseParams
    `withInfo` "Custom reordering mutation parameters")

parseParams :: Parser Parameters
parseParams = P
  <$> argument auto (metavar "PROB_REORDERING")
  <*> argument auto (metavar "PROB_BAD_NEWLINES")
  <*> argument auto (metavar "PROB_MUTATION")

parseProc :: Parser Options
parseProc = Process <$> parseProcOpts

-- XXX "Any combination of" - should be a product, not a sum
parseProcOpts :: Parser ProcOpts
parseProcOpts = subparser $
  command "min" (pure MinTemp `withInfo` "Calculate minimum temp in K") <>
  command "max" (pure MaxTemp `withInfo` "Calculate maximum temp in K") <>
  command "mean" (pure MeanTemp `withInfo` "Calculate mean temp in K") <>
  command "stations"
    (pure StationDistrib `withInfo` "Number of observations per station") <>
  command "distance" (parseDistance `withInfo`
    "Total distance travelled (requires maximum delay assumption in secs)") <>
  command "normalise" (parseNormalise `withInfo`
    "Normalise data into chosen distance and temperature units")

parseDistance :: Parser ProcOpts
parseDistance = Distance <$> argument auto (metavar "MAX_DELAY")

parseNormalise :: Parser ProcOpts
parseNormalise = Normalise <$> dist <*> temp
  where dist = subparser $
          command "km" (pure KM `withInfo` "Kilometres") <>
          command "mi" (pure MI `withInfo` "Miles") <>
          command "me" (pure ME `withInfo` "Metres")
        temp = subparser $
          command "c" (pure C `withInfo` "Celsius") <>
          command "k" (pure K `withInfo` "Kelvin") <>
          command "f" (pure F `withInfo` "Fahrenheit")
