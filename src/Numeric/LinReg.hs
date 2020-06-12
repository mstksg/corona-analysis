{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Numeric.LinReg (
    linReg
  , linRegAt
  -- , logSeries
  -- , expSeries
  , LinReg(..)
  , applyLinReg
  , linRegTrans
  , linRegAtTrans
  , applyLinRegTrans

  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Profunctor
import           Numeric.Transform
import qualified Control.Foldl     as F

logSeries :: (Floating a, Ord a) => [(a, a)] -> [(a, a)]
logSeries = mapMaybe (traverse (fmap log . mfilter (> 0) . Just))

expSeries :: (Functor f, Floating a) => f (a, a) -> f (a, a)
expSeries = (fmap . second) exp

data LinReg a = LR { lrAlpha :: a
                   , lrBeta  :: a
                   }
  deriving (Show, Eq)

linReg
    :: (Foldable t, Fractional a)
    => t (a, a)
    -> (LinReg a, a)
linReg = F.fold $ do
    x  <- lmap fst F.mean
    y  <- lmap snd F.mean
    x2 <- lmap (sq . fst) F.mean
    y2 <- lmap (sq . snd) F.mean
    xy <- lmap (uncurry (*)) F.mean
    pure $
      let cxy = xy - x * y
          vx2 = x2 - sq x
          vy2 = y2 - sq y
          β   = cxy / vx2
          α   = y - β * x
          r2  = sq β * vx2 / vy2
      in  (LR α β, r2)
  where
    sq x = x * x

linRegAt
    :: (Fractional a, Ord a)
    => a           -- ^ minimal R-squared
    -> [(a, a)]
    -> Maybe (LinReg a, (Int, a))
linRegAt r2 = find ((>= r2) . snd . snd)
            . mapMaybe (\xs -> do guard (not (null xs))
                                  pure $ second (length xs,) (linReg xs)
                       )
            . tails

applyLinReg
    :: Num a
    => LinReg a
    -> a
    -> a
applyLinReg LR{..} x = lrAlpha + lrBeta * x

linRegTrans
    :: (Foldable t, Fractional a)
    => Transform a
    -> t (a, a)
    -> (LinReg a, a)
linRegTrans tr = linReg . untransSeries tr . toList

linRegAtTrans
    :: (Fractional a, Ord a)
    => Transform a
    -> a           -- ^ minimal R-squared
    -> [(a, a)]
    -> Maybe (LinReg a, (Int, a))
linRegAtTrans tr r2 = find ((>= r2) . snd . snd)
            . mapMaybe (\xs -> do guard (not (null xs))
                                  pure $ second (length xs,) (linRegTrans tr xs)
                       )
            . tails


applyLinRegTrans
    :: Num a
    => Transform a
    -> LinReg a
    -> a
    -> a
applyLinRegTrans tr LR{..} x = trTo tr $ lrAlpha + lrBeta * x

