module Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString       (IResult (..), Parser (..),
                                                   Result (..))
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.ByteString.Char8            (pack, unpack)
import           Data.Time                        (FormatTime (..), UTCTime,
                                                   defaultTimeLocale,
                                                   parseTimeM)
import           Numeric.Natural
import           System.IO                        as IO

import Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB

import           Types

type Line = (UTCTime, (Natural, Natural), Int, Code)
type Code = ByteString

--------------------------------------------------------------------------------

chunkSize :: Int
chunkSize = 25

parser :: MonadIO m => Producer LogEntry m ()
parser = PB.hGet chunkSize IO.stdin >-> parseLazy ""

parseEffect :: IO ()
parseEffect = runEffect $ PB.hGet 25 IO.stdin >-> parseLazy ""
                            >-> P.take 5 >-> P.print

-- Attoparsec's many1 combinator forces the input stream.
-- ... so, let's wrap it up and produce our own lazy stream with Pipes.
parseLazy :: MonadIO m => ByteString -> Pipe ByteString LogEntry m ()
parseLazy rem = do
  r <- A.parseWith await parseLine rem
  case r of Done rem le  -> yield le >> parseLazy rem
            Partial cb   -> undefined -- XXX Can this occur?
            Fail rem _ _ -> parseLazy rem

--------------------------------------------------------------------------------

codeAu :: Code -> Bool
codeAu = (==) "AU"

codeFr :: Code -> Bool
codeFr = (==) "FR"

codeUS :: Code -> Bool
codeUS = (==) "US"

-- |Produce an entry with correct units from raw digits and an observatory code.
mkEntry :: UTCTime -> (Natural, Natural) -> Integer -> Code -> LogEntry
mkEntry time (lx, ly) temp code = LogEntry time $
  if | codeAu code -> AU  (Location (fromIntegral lx) (fromIntegral ly))
                          (fromIntegral temp)
     | codeFr code -> FR  (Location (fromIntegral lx) (fromIntegral ly))
                          (fromIntegral temp)
     | codeUS code -> US  (Location (fromIntegral lx) (fromIntegral ly))
                          (fromIntegral temp)
     | otherwise -> Other (Location (fromIntegral lx) (fromIntegral ly))
                          (fromIntegral temp)

parseLine :: Parser LogEntry
parseLine = do
  time <- parseTime
  pipe
  loc  <- location
  pipe
  temp <- temperature
  pipe
  code <- obsCode
  A.try newline
  return (mkEntry time loc temp code)

-- yyyy-MM-ddThh:mm in UTC
timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M"

parseTime :: Parser UTCTime
parseTime = do
  str <- A.take 16
  parseTimeM False defaultTimeLocale timeFormat (unpack str)

location :: Parser (Natural, Natural)
location = do
  lx <- A.decimal
  A.char ','
  ly <- A.decimal
  return (lx, ly)

temperature :: Parser Integer
temperature = A.signed A.decimal

obsCode :: Parser Code
obsCode = A.take 2

pipe :: Parser Char
pipe = A.char '|'

newline :: Parser Char
newline = A.char '\n'
