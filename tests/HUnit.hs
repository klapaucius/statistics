import Control.Applicative
import Data.List
import Test.HUnit

import Statistics.Math
import Debug.Trace


-- Exact albeit slow implementation of choose
choose' :: Integer -> Integer -> Integer
choose' n k = factorial' n `div` (factorial' k * factorial' (n-k))

-- Exact implementation of factorial
factorial' :: Integer -> Integer
factorial' n = product [1..n]


-- Error in determination of factorial
factorialErr :: Integer -> Double
factorialErr n = (f' - f) / f'
  where
    f' = fromIntegral (factorial' n)
    f  = factorial (fromIntegral n)

-- Error in determination of log of factorial
factorialLogErr :: Integer -> Double
factorialLogErr n = (f' - f) / f'
  where
    f' = log $ fromIntegral $ factorial' n
    f  = logFactorial (fromIntegral n)

-- Error in determination if binmial coef.
chooseErr :: Integer -> Integer -> Double 
chooseErr n k = (c - c') / c'
  where
    c' = fromIntegral (choose' n k)
    c  = choose (fromIntegral n) (fromIntegral k)

-- Error in logGamma function for integer points > 2
logGammaErr :: Int -> Double
logGammaErr n = (logGamma (fromIntegral n) - l) / l where l = logFactorial n

-- Error in logGammaL function for integer points > 2
logGammaLErr :: Int -> Double
logGammaLErr n = (logGammaL (fromIntegral n) - l) / l where l = logFactorial n
----------------------------------------------------------------
-- Full list of tests
----------------------------------------------------------------

-- These tests may take a while to run
allTests :: Test
allTests = TestList [
    TestCase $ assertBool "Factorial is expected to be precise at 1e-15 level" $
      all (< 1e-15) $ map factorialErr [0..170]
  , TestCase $ assertBool "Factorial is expected to be precise at 1e-15 level" $
      all (< 1e-15) $ map factorialLogErr [2..170]
  , TestCase $ assertBool "logGamma is expected to be precise at 1e-9 level" $
      all (< 1e-9) $ map logGammaErr [3..100000]
  , TestCase $ assertBool "logGammaL is expected to be precise at 1e-15 level" $
      all (< 1e-15) $ map logGammaErr [3..100000]
  -- , TestCase $ assertBool "choose is expected to precise at 1e-14 level" $
      -- all (< 1e-14) [chooseErr n k | n <- [0..1000], k <- [0..n]]
  ]

main :: IO ()
main = print =<< runTestTT allTests