module Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString       (IResult (..), Parser (..),
                                                   Result (..))
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.ByteString.Char8            (pack, unpack)
import           Data.List                        (intercalate)
import           Data.Time                        (FormatTime (..), UTCTime,
                                                   defaultTimeLocale,
                                                   formatTime, parseTimeM)
import           Numeric.Natural
import           System.IO                        as IO

import Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB

import           Types

import Debug.Trace

type Line = (UTCTime, (Natural, Natural), Int, Code)
type Code = ByteString

--------------------------------------------------------------------------------

-- XXX Kinda arbitrary
chunkSize :: Int
chunkSize = 25

parser :: MonadIO m => Producer LogEntry m ()
parser = PB.hGet chunkSize IO.stdin >-> parseLazy ""

-- Attoparsec isn't lazy enough, but Pipes are nice.
parseLazy :: Monad m => ByteString -> Pipe ByteString LogEntry m ()
parseLazy rem = do
  r <- A.parseWith await parseLine rem
  case r of Done rem le  -> yield le >> parseLazy rem
            Fail rem _ _ -> parseLazy rem
            Partial cb   -> error "parseWith invariant failed!"

prettyPipe :: Monad m => Bool -> Pipe LogEntry ByteString m ()
prettyPipe ln = forever $ do
  next <- await
  yield . pack $ if ln then pretty next ++ "\n" else pretty next

--------------------------------------------------------------------------------

pretty :: LogEntry -> String
pretty (LogEntry time (Measurement st (Location x y) temp)) =
  intercalate "|" [ti, loc, te, sta]
  where ti  = formatTime defaultTimeLocale timeFormat time
        loc = show (round x) ++ "," ++ show (round y)
        te  = show (round temp)
        sta = show st


-- |Produce an entry with correct units from raw digits and an observatory code.
mkEntry :: UTCTime -> (Natural, Natural) -> Integer -> Station -> LogEntry
mkEntry time (lx, ly) temp sta = LogEntry time
  $ mkMeasure sta (fromIntegral lx, fromIntegral ly) (fromIntegral temp)

parseLine :: Parser LogEntry
parseLine = do
  time <- parseTime
  pipe
  loc  <- location
  pipe
  temp <- temperature
  pipe
  sta  <- station
  A.many' newline
  return (mkEntry time loc temp sta)

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

-- Allow only ASCII alphabet in stations
station :: Parser Station
station = do
  a <- A.satisfy A.isAlpha_ascii
  b <- A.satisfy A.isAlpha_ascii
  let str = [a, b]
  return $ if | str == "AU" -> AUS
              | str == "US" -> USA
              | str == "FR" -> FRA
              | otherwise   -> Other str

pipe :: Parser Char
pipe = A.char '|'

newline :: Parser Char
newline = A.char '\n'
