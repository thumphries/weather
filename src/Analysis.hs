module Analysis where

import           Control.Monad
import           Control.Monad.Identity
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Pipes                  as P
import           Pipes.Prelude          as P

import           Parser
import           Types

-- - the total distance travelled
-- - normalised output

kelvinPipe :: MonadIO m => Pipe LogEntry Kelvin m ()
kelvinPipe = forever $ do
  (LogEntry _ (Measurement _ _ temp)) <- await
  yield (toKelvin temp)

minTemp :: MonadIO m => Producer LogEntry m () -> m (Maybe Kelvin)
minTemp p = P.minimum (p >-> kelvinPipe)

maxTemp :: MonadIO m => Producer LogEntry m () -> m (Maybe Kelvin)
maxTemp p = P.maximum (p >-> kelvinPipe)

meanTemp :: MonadIO m => Producer LogEntry m () -> m (Maybe Kelvin)
meanTemp p = P.fold (\(!i, !k1) k2 -> (i+1, k1+k2))
                    (0, Kelvin 0) combine (p >-> kelvinPipe)
  where combine :: (Integer, Kelvin) -> Maybe Kelvin
        combine (0, _) = Nothing
        combine (i, k) = Just (k / fromIntegral i)

obsPipe :: MonadIO m => Pipe LogEntry String m ()
obsPipe = forever $ do
  (LogEntry _ (Measurement s _ _)) <- await
  yield (Prelude.show s)

obsCount :: MonadIO m => Producer LogEntry m () -> m (Map String Integer)
obsCount p = P.fold (\m s -> M.alter fun s m) M.empty id (p >-> obsPipe)
  where fun :: Maybe Integer -> Maybe Integer
        fun Nothing = Just 1
        fun (Just !i) = Just (i + 1)
