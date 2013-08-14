{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Statistics.Sample.Foldl
-- Copyright : (c) 2008 Don Stewart, 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Commonly used sample statistics, also known as descriptive
-- statistics.

module Statistics.Sample.Foldl where

import Statistics.Types (Sample,WeightedSample)
import qualified Data.Vector.Generic as G
import Control.Foldl (Fold(..))
import Control.Applicative ((<$>), (<*>))

-- Operator ^ will be overriden
import Prelude hiding ((^))

-- | Apply a strict left 'Fold' to a vector and extract the final result
fold :: (G.Vector v a) => Fold a b -> v a -> b
fold (Fold go begin fini) xs = fini (G.foldl' go begin xs)
{-# INLINE fold #-}

-- | Compute the minimum of a vector.
findMin :: Fold Double Double
findMin = Fold go (1/0) id
  where
    go lo k = min lo k
{-# INLINE findMin #-}

-- | Compute the maximum of a vector.
findMax :: Fold Double Double
findMax = Fold go (-1/0) id
  where
    go hi k = max hi k
{-# INLINE findMax #-}

-- | /O(n)/ Range. The difference between the largest and smallest
-- elements of a sample.
range :: Fold Double Double
range = (-) <$> findMax <*> findMin
{-# INLINE range #-}

-- | /O(n)/ Arithmetic mean.  This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
mean :: Fold Double Double
mean = Fold go (T 0 0) fini
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
        where m' = m + (x - m) / fromIntegral n'
              n' = n + 1
{-# INLINE mean #-}

-- | /O(n)/ Arithmetic mean for weighted sample. It uses a single-pass
-- algorithm analogous to the one used by 'mean'.
meanWeighted :: Fold (Double,Double) Double
meanWeighted = Fold go (V 0 0) fini
    where
      fini (V a _) = a
      go (V m w) (x,xw) = V m' w'
          where m' | w' == 0   = 0
                   | otherwise = m + xw * (x - m) / w'
                w' = w + xw
{-# INLINE meanWeighted #-}

-- | /O(n)/ Harmonic mean.  This algorithm performs a single pass over
-- the sample.
harmonicMean :: Fold Double Double
harmonicMean = Fold go (T 0 0) fini
  where
    fini (T b a) = fromIntegral a / b
    go (T x y) n = T (x + (1/n)) (y+1)
{-# INLINE harmonicMean #-}

-- | /O(n)/ Geometric mean of a sample containing no negative values.
geometricMean :: Fold Double Double
geometricMean = modify mean
    where
      modify (Fold go' begin fini) = Fold go begin (exp . fini) 
        where 
          go s x = go' s (log x)
{-# INLINE geometricMean #-}


data V = V {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- don't support polymorphism, as we can't get unboxed returns if we use it.
data T = T {-# UNPACK #-}!Double {-# UNPACK #-}!Int

data T1 = T1 {-# UNPACK #-}!Int {-# UNPACK #-}!Double {-# UNPACK #-}!Double

------------------------------------------------------------------------
-- Helper code. Monomorphic unpacked accumulators.

-- (^) operator from Prelude is just slow.
(^) :: Double -> Int -> Double
x ^ 1 = x
x ^ n = x * (x ^ (n-1))
{-# INLINE (^) #-}


-- $references
--
-- * Chan, T. F.; Golub, G.H.; LeVeque, R.J. (1979) Updating formulae
--   and a pairwise algorithm for computing sample
--   variances. Technical Report STAN-CS-79-773, Department of
--   Computer Science, Stanford
--   University. <ftp://reports.stanford.edu/pub/cstr/reports/cs/tr/79/773/CS-TR-79-773.pdf>
--
-- * Knuth, D.E. (1998) The art of computer programming, volume 2:
--   seminumerical algorithms, 3rd ed., p. 232.
--
-- * Welford, B.P. (1962) Note on a method for calculating corrected
--   sums of squares and products. /Technometrics/
--   4(3):419&#8211;420. <http://www.jstor.org/stable/1266577>
--
-- * West, D.H.D. (1979) Updating mean and variance estimates: an
--   improved method. /Communications of the ACM/
--   22(9):532&#8211;535. <http://doi.acm.org/10.1145/359146.359153>
