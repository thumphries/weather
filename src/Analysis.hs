module Analysis where

import           Control.Monad
import           Control.Monad.Identity
import           Data.Heap              (Entry(..), Heap)
import qualified Data.Heap              as H
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Traversable       as T
import           Data.Time
import           Pipes                  as P
import           Pipes.Prelude          as P

import           Parser
import           Types

-- - normalised output

kelvinPipe :: Monad m => Pipe LogEntry Kelvin m ()
kelvinPipe = forever $ do
  (LogEntry _ (Measurement _ _ temp)) <- await
  yield (toKelvin temp)

metresPipe :: Monad m => Pipe LogEntry (Location Metres) m ()
metresPipe = forever $ do
  (LogEntry _ (Measurement _ (Location x y) _)) <- await
  yield (Location (toMetres x) (toMetres y))


minTemp :: Monad m => Producer LogEntry m () -> m (Maybe Kelvin)
minTemp p = P.minimum (p >-> kelvinPipe)

maxTemp :: Monad m => Producer LogEntry m () -> m (Maybe Kelvin)
maxTemp p = P.maximum (p >-> kelvinPipe)

meanTemp :: Monad m => Producer LogEntry m () -> m (Maybe Kelvin)
meanTemp p = P.fold (\(!i, !k1) k2 -> (i+1, k1+k2))
                    (0, Kelvin 0) combine (p >-> kelvinPipe)
  where combine :: (Integer, Kelvin) -> Maybe Kelvin
        combine (0, _) = Nothing
        combine (i, k) = Just (k / fromIntegral i)

obsPipe :: Monad m => Pipe LogEntry String m ()
obsPipe = forever $ do
  (LogEntry _ (Measurement s _ _)) <- await
  yield (Prelude.show s)

obsCount :: Monad m => Producer LogEntry m () -> m (Map String Integer)
obsCount p = P.fold (\m s -> M.alter fun s m) M.empty id (p >-> obsPipe)
  where fun :: Maybe Integer -> Maybe Integer
        fun Nothing = Just 1
        fun (Just !i) = Just (i + 1)

-- XXX Could hold onto entries when empty. Needs a way to flush on empty
orderPipe :: Monad m => Integer -> Pipe LogEntry LogEntry m ()
orderPipe maxDelay = go H.empty
  where go :: Monad m => Heap (Entry UTCTime Measurement)
           -> Pipe LogEntry LogEntry m ()
        go heap = do
          (LogEntry time m) <- await
          let heap'  = H.insert (Entry time m) heap
              (a, b) = flush time heap'
          _ <- H.mapM (\(Entry t m) -> yield (LogEntry t m)) a
          go b
        flush bound = H.partition
          (\(Entry t _) -> abs (diffUTCTime bound t) >= fromIntegral maxDelay)

distance :: Monad m => Integer -> Producer LogEntry m () -> m Metres
distance n p = P.fold
  (\(p1, d) p2 -> (p2, pointwiseDistance p1 p2)) (Location 0 0, 0) snd
  -- (p >-> metresPipe)
  (p >-> orderPipe n >-> metresPipe)

pointwiseDistance :: Location Metres -> Location Metres -> Metres
pointwiseDistance (Location x1 y1) (Location x2 y2) = Metres dist
  where squx = (x1 - x2) ^ 2
        squy = (y1 - y2) ^ 2
        dist = sqrt (realToFrac (squx + squy))
