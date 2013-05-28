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
         in mappend x x
    | otherwise = x `mappend` mprod (n-1) x

data Channel = Channel
    { smoothing         :: !Double
    , maxHistory        :: !Int
    , history           :: !(IORef History)
    , current           :: !(IORef Double)
    }

newChannel :: Double -> Int -> IO Channel
newChannel smooth hist = 
    Channel smooth hist <$> newIORef FT.empty <*> newIORef 0

updateHistory :: Int -> ChanSample -> History -> History
updateHistory n x xs = FT.dropUntil ((<= n) . p1count) (xs |> x)

updateCurrent smooth x y = x * lambda + y * (1 - lambda)
    where lambda = exp (-smooth)

feedChannel :: Channel -> ChanSample -> IO Double
feedChannel chan sample@(ChanSample _ x) = do
    modifyIORef' (history chan) (updateHistory (maxHistory chan) sample)
    modifyIORef' (current chan) (updateCurrent (smoothing  chan) x)
    readChannel chan

readChannel chan = do
    x    <- readIORef (current chan)
    hist <- readIORef (history chan)
    let stats = measure hist
        dist = normalDistr (mean stats) (max 1 (fromMaybe 1 (stddev stats)))
    return (cumulative dist x)
