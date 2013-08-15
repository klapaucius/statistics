import qualified Statistics.Sample as S

import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as U

import System.Random.MWC
import Criterion.Main

import qualified Statistics.Sample as S
import qualified Statistics.Sample.Foldl as F
import Statistics.Transform

-- Test sample
sample :: U.Vector Double
sample = runST $ flip uniformVector 10000 =<< create

-- Weighted test sample
sampleW :: U.Vector (Double,Double)
sampleW = U.zip sample (U.reverse sample)

-- Simple benchmark for functions from Statistics.Sample.Foldl
main :: IO ()
main =
  defaultMain
  [ bgroup "sample"
    [ bench "mean"             $ nf (\x -> S.mean x)             sample  
    , bench "range-s"            $ nf (\x -> S.range x)            sample
    , bench "range-f"            $ nf (\x -> F.fold F.range x)     sample
    --   -- Mean
    -- , bench "mean"             $ nf (\x -> mean x)             sample
    -- , bench "meanWeighted"     $ nf (\x -> meanWeighted x)     sampleW
    -- , bench "harmonicMean"     $ nf (\x -> harmonicMean x)     sample
    -- , bench "geometricMean"    $ nf (\x -> geometricMean x)    sample
    --   -- Variance
    -- , bench "variance"         $ nf (\x -> variance x)         sample
    -- , bench "varianceUnbiased" $ nf (\x -> varianceUnbiased x) sample
    -- , bench "varianceWeighted" $ nf (\x -> varianceWeighted x) sampleW
    --   -- Other
    -- , bench "stdDev"           $ nf (\x -> stdDev x)           sample
    -- , bench "skewness"         $ nf (\x -> skewness x)         sample
    -- , bench "kurtosis"         $ nf (\x -> kurtosis x)         sample
    --   -- Central moments
    -- , bench "C.M. 2"           $ nf (\x -> centralMoment 2 x)  sample
    -- , bench "C.M. 3"           $ nf (\x -> centralMoment 3 x)  sample
    -- , bench "C.M. 4"           $ nf (\x -> centralMoment 4 x)  sample
    -- , bench "C.M. 5"           $ nf (\x -> centralMoment 5 x)  sample
    ]
  ]

