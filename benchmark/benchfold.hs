import Statistics.Sample.Foldl
import qualified Statistics.Sample as S

import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as U

import System.Random.MWC
-- import Criterion.Main

-- Test sample
sample :: U.Vector Double
sample = runST $ flip uniformVector 10000 =<< create

main = do
    print $ fold range sample
    -- print $ S.range sample
