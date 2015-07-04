module Generator where

import           Control.Applicative
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime,
                                        posixSecondsToUTCTime)
import           Pipes                 as P
import           Pipes.Prelude         as P
import           System.Random

import           Types

data Parameters = P
  { pOrdered :: Double
  , pNewline :: Double
  , pInsert  :: Double
  , pDelete  :: Double
  } deriving (Show, Eq, Ord)

-- Public interface.
perfectInput :: MonadIO m => Producer String m ()
perfectInput = paramStream p
  where p = P { pOrdered = 0
              , pNewline = 0
              , pInsert  = 0
              , pDelete  = 0
              }

-- Stream of semi-garbled input
realisticInput :: MonadIO m => Producer String m ()
realisticInput = paramStream p
  where p = P { pOrdered = 0.1
              , pNewline = 0.05
              , pInsert  = 0.05
              , pDelete  = 0.05
              }

-- Stream of very garbled input
nightmareInput :: MonadIO m => Producer String m ()
nightmareInput = paramStream p
  where p = P { pOrdered = 0.5
              , pNewline = 0.5
              , pInsert  = 0.5
              , pDelete  = 0.5
              }

paramStream :: MonadIO m => Parameters -> Producer String m ()
paramStream p = do
  g <- liftIO getStdGen
  let (g1, g')    = split g
      (g2, g'')   = split g'
      (g3, g''')  = split g''
      (g4, g'''') = split g'''
      (g5, _)     = split g''''
  cleanMeasure g1 >-> orderedEntries g2
                  >-> reorderedEntries (pOrdered p) g3
                  >-> prettyEntries
                  >-> newlineChaos (pNewline p) g4
                  >-> mutateEntry (pInsert p) (pDelete p) g5

-- XXX kinda arbitrary
maxInt :: Integer
maxInt = 4294967296

-- First we need a stream of "Correct" log entries
cleanMeasure :: Monad m => StdGen -> Producer Measurement m ()
cleanMeasure  g = do
  let rand0 x g = randomR (0, x) g :: (Integer, StdGen)
      (x, g1)   = rand0 maxInt g
      (y, g2)   = rand0 maxInt g1
      (t, g3)   = rand0 maxInt g2
      (s, g4)   = rand0 3      g3
      (c1, g5)  = randomR ('A', 'Z') g4
      (c2, g6)  = randomR ('A', 'Z') g5
      station   = if | s == 0    -> AUS
                     | s == 1    -> FRA
                     | s == 2    -> USA
                     | otherwise -> Other [c1, c2]
  yield (mkMeasure station (fromIntegral x, fromIntegral y) (fromIntegral t))
  cleanMeasure g6

-- ... Then we need to give them timestamps
orderedEntries :: Monad m => StdGen -> Pipe Measurement LogEntry m ()
orderedEntries g =
  let (initialTime, g2) = randomR (0, maxInt) g :: (Integer, StdGen)
  in  go (fromIntegral initialTime) g2
  where go :: Monad m => POSIXTime -> StdGen -> Pipe Measurement LogEntry m ()
        go last h = do
          m <- await
          let (diff, h1) = randomR (0, maxInt) h :: (Integer, StdGen)
              newTime    = last + fromIntegral diff
          yield (LogEntry { leTime = posixSecondsToUTCTime newTime
                          , leMeasure = m
                          })
          go newTime h1

-- ... Optionally, we reorder them (use some bounded queue)
reorderedEntries :: (RandomGen g, Monad m)
                 => Double -> g -> Pipe LogEntry LogEntry m ()
reorderedEntries = undefined

-- ... Then we turn them into text
prettyEntries :: Monad m => Pipe LogEntry String m ()
prettyEntries = undefined

-- ... Then we garble them arbitrarily. This one will combine lines
-- at random.
newlineChaos :: (RandomGen g, Monad m)
             => Double -> g -> Pipe String String m ()
newlineChaos = undefined

-- ... remove some characters, add other characters (low probability)
mutateEntry :: (RandomGen g, Monad m) => Double -> Double -> g
            -> Pipe String String m ()
mutateEntry = undefined
