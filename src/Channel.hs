{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Channel where

import Control.Applicative
import Spectrum
import Data.IORef
import Data.FingerTree as FT
import Data.Maybe
import Data.Monoid
import Data.Word
import Math.Statistics.Monoid
import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal
import qualified Data.Vector.Unboxed as U

type History = FingerTree (Pass1 Double) ChanSample
data ChanSample = ChanSample !Int !Double
instance Measured (Pass1 Double) ChanSample where
    measure (ChanSample n x) = mprod n (pass1 x)

mprod 0 _ = mempty
mprod 1 x = x
mprod n x
    | even n    = 
        let x' = mprod (n `div` 2) x
         in mappend x' x'
    | otherwise = x `mappend` mprod (n-1) x

data Channel = Channel
    { attack            :: !Double
    , decay             :: !Double
    , maxHistory        :: !Int
    , history           :: !(IORef History)
    , current           :: !(IORef Double)
    }

newChannel :: Double -> Double -> Int -> IO Channel
newChannel atk dcy hist = 
    Channel atk dcy hist <$> newIORef FT.empty <*> newIORef 0

updateHistory :: Int -> ChanSample -> History -> History
updateHistory n x xs = FT.takeUntil ((>= n) . p1count) (x <| xs)

updateCurrent atk dcy (ChanSample n x) y
    | isNaN y   = x
    | otherwise = x * alpha + y * beta
    where 
        alpha = exp (negate (fromIntegral n * lambda))
        beta  = 1 - alpha
        lambda = if x > y then atk else dcy

feedChannel :: Channel -> ChanSample -> IO Double
feedChannel chan sample = do
    modifyIORef' (history chan) (updateHistory (maxHistory chan) sample)
    modifyIORef' (current chan) (updateCurrent (attack chan) (decay chan) sample)
    readChannel chan

-- TODO: separate auto-eq from auto-volume
-- TODO: filter the post-normalization value
-- even better if filtering is done by an independent system
readChannel chan = do
    x    <- readIORef (current chan)
    hist <- readIORef (history chan)
    let stats = measure hist
        dist = normalDistr (mean stats) (max 1 (fromMaybe 1 (stddev stats)))
    return $! cumulative dist x
