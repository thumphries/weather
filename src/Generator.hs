module Generator where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (cons, pack, unpack)
import           Data.List             as L (drop, length, take)
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime,
                                        posixSecondsToUTCTime)
import           Pipes                 as P
import           Pipes.Prelude         as P
import           System.Random

import           Parser
import           Types

data Parameters = P
  { pOrdered :: Double
  , pNewline :: Double
  , pMutate  :: Double
  } deriving (Show, Eq, Ord)

-- Public interface.
perfectInput :: MonadIO m => Producer ByteString m ()
perfectInput = paramStream p
  where p = P { pOrdered = 0
              , pNewline = 0
              , pMutate  = 0
              }

-- Stream of semi-garbled input
realisticInput :: MonadIO m => Producer ByteString m ()
realisticInput = paramStream p
  where p = P { pOrdered = 0.1
              , pNewline = 0.05
              , pMutate  = 0.000005
              }

-- Stream of very garbled input
nightmareInput :: MonadIO m => Producer ByteString m ()
nightmareInput = paramStream p
  where p = P { pOrdered = 0.5
              , pNewline = 0.5
              , pMutate  = 0.5
              }

paramStream :: MonadIO m => Parameters -> Producer ByteString m ()
paramStream p = do
  g <- liftIO getStdGen
  let (g1, g')    = split g
      (g2, g'')   = split g'
      (g3, g''')  = split g''
      (g4, g'''') = split g'''
      (g5, _)     = split g''''
  cleanMeasure g1 >-> orderedEntries g2
                  >-> reorderedEntries (pOrdered p) g3
                  >-> prettyPipe
                  >-> newlineChaos (pNewline p) g4
                  >-> mutateEntry (pMutate p) g5

-- XXX kinda arbitrary bound on our random integers
maxInt :: Integer
maxInt = 4294967296

-- XXX arbitrary
medInt :: Integer
medInt = 10000

-- XXX also arbitrary
smallInt :: Integer
smallInt = 720

-- First we need a stream of "Correct" log entries
cleanMeasure :: Monad m => StdGen -> Producer Measurement m ()
cleanMeasure  g = do
  let rand0 x g = randomR (0, x) g :: (Integer, StdGen)
      (x, g1)   = rand0 medInt g
      (y, g2)   = rand0 medInt g1
      (t, g3)   = rand0 smallInt g2
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
  -- Initial time is bounded between 1970 and 2106.
  -- Each step is between 0 and smallInt seconds.
  let (initialTime, g2) = randomR (0, maxInt) g :: (Integer, StdGen)
  in  go (fromIntegral initialTime) g2
  where go :: Monad m => POSIXTime -> StdGen -> Pipe Measurement LogEntry m ()
        go last h = do
          m <- await
          let (diff, h1) = randomR (0, smallInt) h :: (Integer, StdGen)
              newTime    = last + fromIntegral diff
          yield (LogEntry { leTime = posixSecondsToUTCTime newTime
                          , leMeasure = m
                          })
          go newTime h1

-- ... Optionally, we reorder them (use some bounded queue)
reorderedEntries :: Monad m => Double -> StdGen -> Pipe LogEntry LogEntry m ()
reorderedEntries r g = go Nothing g
  where go stashed g = do
          next <- await
          let (f, g1) = randomR (0, 1) g :: (Double, StdGen)
          if f >= r
            then yield next >> go stashed g1
            else maybe (return ()) yield stashed >> go (Just next) g1

-- ... Give them between 0 and 3 newlines as separation...
newlineChaos :: Monad m => Double -> StdGen -> Pipe ByteString ByteString m ()
newlineChaos r g = do
  next <- await
  let (f, g1) = randomR (0, 1) g  :: (Double, StdGen)
      (c, g2) = randomR (0, 3) g1 :: (Int, StdGen)
  if f >= r
    then yield $ next `B.append` "\n"
    else yield $ next `B.append` pack (L.take c (repeat '\n'))
  newlineChaos r g2

-- ... Mutate. remove some characters, add other characters.
mutateEntry :: Monad m => Double -> StdGen -> Pipe ByteString ByteString m ()
mutateEntry r g = do
  next <- await
  let (f, g1) = randomR (0, 1) g  :: (Double, StdGen)
      (c, g2) = randomR (0, 5) g1 :: (Int, StdGen)
      mutated = mutate (B.length next) c g2 next
  if f >= r then yield mutated else yield next
  mutateEntry r (fst (split g2))
  where mutate :: Int -> Int -> StdGen -> ByteString -> ByteString
        mutate _   0 _ s = s
        mutate len n h s =
         let (f,   h1) = randomR (0, r)     h  :: (Double, StdGen)
             (pos, h2) = randomR (0, len)   h1 :: (Int, StdGen)
             (act, h3) = randomR (0, 2)     h2 :: (Int, StdGen)
             (ins, h4) = randomR (' ', '~') h3 :: (Char, StdGen)
             (a, b)    = B.splitAt pos s
             (len', warped) =
               if | act == 0 ->                         -- Delete
                      (len - 1, a `B.append` B.drop 1 b)
                  | act == 1 ->                         -- Replace
                      (len, a `B.append` (ins `cons` B.drop 1 b))
                  | act == 2 ->                         -- Insert
                      (len + 1, a `B.append` (ins `cons` b))
                  | otherwise -> error "randomR invariant broke!"
         in if f >= r
            then mutate len  (n - 1) h4 s
            else mutate len' (n - 1) h4 warped
