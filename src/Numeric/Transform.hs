
module Numeric.Transform (
    Transform(..), (.%)
  , idTrans, expTrans, recipTrans, mulTrans, addTrans, logisticTrans
  , transSeries
  , untransSeries
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Maybe

data Transform a = Tr
          { trTo   :: a -> a
          , trFrom :: a -> Maybe a
          }

(.%)
    :: Transform a
    -> Transform a
    -> Transform a
Tr f g .% Tr f' g' = Tr (f . f') (g' <=< g)
infixr 9 .%

idTrans :: Transform a
idTrans = Tr id Just

expTrans :: (Floating a, Ord a) => Transform a
expTrans = Tr exp (fmap log . mfilter (>= 0) . Just)

recipTrans :: Fractional a => Transform a
recipTrans = Tr recip (Just . recip)

mulTrans :: Fractional a => a -> Transform a
mulTrans a = Tr (* a) (Just . (/ a))

addTrans :: Num a => a -> Transform a
addTrans a = Tr (+ a) (Just . subtract a)

logisticTrans :: (Floating a, Ord a) => a -> Transform a
logisticTrans m = mulTrans m
               .% recipTrans
               .% addTrans 1
               .% expTrans

untransSeries :: Transform a -> [(a, a)] -> [(a, a)]
untransSeries = mapMaybe . traverse . trFrom

transSeries :: Functor f => Transform a -> f (a, a) -> f (a, a)
transSeries = (fmap . second) . trTo
