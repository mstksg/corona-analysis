{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module COVID19.Analyze (
    Extrapolated(..)
  , extrapolateData
  , extrapolateDataTrans
  , logisticHump
  , logisticReg
  -- , logProb
  -- , LogReg(..)
  -- , runLogReg, diffLogReg
  -- , logRegProb
  ) where

import           COVID19.Data
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.Functor
import           Data.List
import           Data.Map          (Map)
import           Data.Maybe
import           Data.Ord
import           Data.Profunctor
import           Data.Semigroup
import           Data.Time
import           Numeric.LinReg
import           Numeric.Natural
import           Numeric.Transform
import qualified Control.Foldl     as F
import qualified Data.Map          as M
import qualified Data.Set          as S

data Extrapolated = Extrap
        { extrapData :: Map Day Double
        , extrapLR   :: LinReg Double
        , extrapTail :: Int
        , extrapR2   :: Double
        }
  deriving Show

extrapolateDataTrans
    :: Transform Double         -- ^ transform
    -> Double                   -- ^ minimal R2
    -> Int                      -- ^ minimal tail
    -> Day                      -- ^ target day
    -> Map Day Natural
    -> Maybe Extrapolated
extrapolateDataTrans tr goal mintail targ inp = do
    (lr, (l, r2)) <- linRegAtTrans tr goal
                   . dataToSeries
                   $ inp
    (day0, _) <- M.lookupMin inp
    guard $ l >= mintail
    pure $ Extrap
      { extrapData = M.fromSet (applyLinRegTrans tr lr . fromIntegral . shiftDay)
                        (S.fromList [day0 .. targ])
      , extrapLR   = lr
      , extrapTail = l
      , extrapR2   = r2
      }


extrapolateData
    :: Double                   -- ^ minimal R2
    -> Int                      -- ^ minimal tail
    -> Day                      -- ^ target day
    -> Map Day Natural
    -> Maybe Extrapolated
extrapolateData = extrapolateDataTrans expTrans

logisticHump
    :: Double             -- ^ minimal R2
    -> Double             -- ^ theoretical cap
    -> Map Day Natural
    -> Maybe (LinReg Double, Day)     -- ^ turnaround/hump day
logisticHump l cap inp = guard (r2 >= l)
                     $> (lr, (unshiftDay . round $ - lrAlpha / lrBeta))
  where
    (lr@LR{..}, r2) = logisticReg cap inp

logisticReg
    :: Double             -- ^ theoretical cap
    -> Map Day Natural
    -> (LinReg Double, Double)
logisticReg cap inp = linRegTrans (logisticTrans cap)
                    . dataToSeries
                    $ inp


-- logProb
--     :: (Foldable t, Floating a)
--     => (a -> a)         -- ^ mean
--     -> (a -> a)         -- ^ variance
--     -> t (a, a)
--     -> a
-- logProb μ σ = getSum . foldMap (Sum . p)
--   where
--     p (x, y) = (- sq ((y - μ x) / σx) / 2) - log (σx * sqrt (2 * pi))
--       where
--         σx = σ x
--     sq x = x * x

-- data LogReg a = LoR { lorMax  :: !a
--                     , lorRate :: !a
--                     , lorMid  :: !a
--                     }

-- runLogReg :: Floating a => LogReg a -> a -> a
-- runLogReg LoR{..} x = lorMax / (1 + exp (- lorRate * (x - lorMid)))

-- diffLogReg :: Floating a => LogReg a -> a -> a
-- diffLogReg LoR{..} x = lorRate * lorMax * ekxm / sq (ekxm + 1)
--   where
--     ekxm = exp (lorRate * (x - lorMid))
--     sq q = q * q

-- logRegProb
--     :: (Foldable t, Floating a)
--     => LogReg a
--     -> t (a, a)
--     -> a
-- logRegProb lr = logProb (runLogReg lr) ((* scale) . diffLogReg lr)
--   where
--     -- scale is picked so that at maximum it will be 0.5 times full width
--     scale = 0.01 * (lorMax lr / diffLogReg lr (lorMid lr))

