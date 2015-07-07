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

tempPipe :: (Monad m, Temperature t) => Pipe LogEntry t m ()
tempPipe = forever $ do
  (LogEntry _ (Measurement _ _ temp)) <- await
  yield (tconvert temp)

locPipe :: (Monad m, Length l) => Pipe LogEntry (Location l) m ()
locPipe = forever $ do
  (LogEntry _ (Measurement _ (Location x y) _)) <- await
  yield (Location (lconvert x) (lconvert y))

normTempPipe :: Monad m => TempUnit -> Pipe LogEntry LogEntry m ()
normTempPipe u = forever $ do
  (LogEntry ti (Measurement st loc te)) <- await
  yield . LogEntry ti $ case u of 
    C -> Measurement st loc (tconvert te :: Celsius)
    K -> Measurement st loc (tconvert te :: Kelvin)
    F -> Measurement st loc (tconvert te :: Fahrenheit)

normLocPipe :: Monad m => DistUnit -> Pipe LogEntry LogEntry m ()
normLocPipe u = forever $ do
  (LogEntry ti (Measurement st (Location x y) te)) <- await
  yield . LogEntry ti $ case u of
    ME -> Measurement st (Location (lconvert x) (lconvert y) :: Location Metres) te
    MI -> Measurement st (Location (lconvert x) (lconvert y) :: Location Miles) te
    KM -> Measurement st (Location (lconvert x) (lconvert y) :: Location Kilometres) te

minTemp :: (Temperature t, Monad m) => Producer LogEntry m () -> m (Maybe t)
minTemp p = P.minimum (p >-> tempPipe)

maxTemp :: (Temperature t, Monad m) => Producer LogEntry m () -> m (Maybe t)
maxTemp p = P.maximum (p >-> tempPipe)

meanTemp :: (Monad m, Temperature t) => Producer LogEntry m () -> m (Maybe t)
meanTemp p = P.fold (\(!i, !k1) k2 -> (i+1, k1+k2))
                    (0, 0) combine (p >-> tempPipe)
  where combine :: RealFrac t => (Integer, t) -> Maybe t
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

orderPipe' :: Monad m => Integer -> Producer LogEntry m () -> Producer LogEntry m ()
orderPipe' i p = do
  undefined
  -- use head and yield
  -- mapM_ yield list

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

distance :: (Monad m, Length l) => Integer -> Producer LogEntry m () -> m l
distance n p = P.fold
  (\(p1, d) p2 -> (p2, pointwiseDistance p1 p2)) (Location 0 0, 0) snd
  (p >-> orderPipe n >-> locPipe)

pointwiseDistance :: Length l => Location l -> Location l -> l
pointwiseDistance (Location x1 y1) (Location x2 y2) = realToFrac dist
  where squx = (x1 - x2) ^ 2
        squy = (y1 - y2) ^ 2
        dist = sqrt (realToFrac (squx + squy))
