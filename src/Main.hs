module Main where

import Options.Applicative
import Pipes               as P
import Pipes.Prelude       as P
import Pipes.ByteString    as PB (stdout)

import Analysis
import Generator
import Parser
import Types

data Options
  = Generate Int GenOpts
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
  | Count
  deriving (Show)

main :: IO ()
main = run =<< execParser (options `withInfo` "Weather balloon megaprocessor")

run :: Options -> IO ()
run (Generate n opts) =
  let generator = case opts of Perfect   -> perfectInput
                               Realistic -> realisticInput
                               Nightmare -> nightmareInput
                               Custom p  -> paramStream p
  in runEffect $ generator >-> P.take n >-> PB.stdout
run (Process opts) = case opts of
  MinTemp        -> (minTemp  parser :: IO (Maybe Kelvin)) >>= Prelude.print
  MaxTemp        -> (maxTemp  parser :: IO (Maybe Kelvin)) >>= Prelude.print
  MeanTemp       -> (meanTemp parser :: IO (Maybe Kelvin)) >>= Prelude.print
  StationDistrib -> obsCount parser                        >>= Prelude.print
  Distance s     -> (distance s parser :: IO Metres)       >>= Prelude.print
  Normalise d t  -> undefined
  Count          -> P.length parser                        >>= Prelude.print

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
    "Normalise data into chosen distance and temperature units") <>
  command "count" (pure Count `withInfo` "Count valid parses")

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
