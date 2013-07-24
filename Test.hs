{-# LANGUAGE BangPatterns #-}
module Main where

import ColorOrgan
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Complex
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as ST
import Spectrum
import VTree
import Channel
import Crossover
import Control.Concurrent.Thread (result)
import Control.Concurrent.Thread.Group

-- some window functions to play with
triangular j n = 1 - abs ((2 * j - n) / (n+1))
hann j n = 0.5 * (1 - cos (2 * pi * (j+1) / (n+2)))

fftSize = 2^10
window f = ST.fromList
    [ f (fromIntegral i) (fromIntegral fftSize - 1)
    | i <- [0 .. fftSize-1]
    ]
sampleRate = 48000
captureBufSize = 2^12

atk = 0.09
dcy = 0.6
tuningPeriod = 120

cutoff = 0.25
clip !x = max 0 (min 1 ((x - cutoff) / (1 - cutoff)))
ramp !m !x = round (m * clip x)

xOver = crossover [(15, 5), (250, 80), (1200, 250), (3200, 250), (5000, 1000), (15000, 5000)]

{-# INLINE dot #-}
dot !v1 !v2 = U.sum (U.zipWith (*) v1 v2)

{-# INLINE magSq #-}
magSq ((!a) :+ (!b)) = a*a + b*b

{-# INLINE sample #-}
sample !n !wts !fs = ChanSample n (dot wts (U.map magSq fs))

main = do
    s <- newSpectrum sampleRate fftSize (Just (window hann))
    c <- newColorOrgan sampleRate captureBufSize
    v <- openVTree "/dev/cu.usbmodem12341"
    
    freqs <- getBinFrequencies s
    let [rWt, gWt, _, _, bWt] = map (tabulate freqs) xOver
    
    rChan <- newChannel atk dcy (round (sampleRate * tuningPeriod))
    gChan <- newChannel atk dcy (round (sampleRate * tuningPeriod))
    bChan <- newChannel atk dcy (round (sampleRate * tuningPeriod))
    
    tgrp1 <- new
    forever $ do
        !n  <- tickColorOrgan c s
        !fs <- getSpectrum s
        
        wait tgrp1 -- there can be only one!
        forkIO tgrp1 $ do
            tgrp2 <- new
            
            (_, b) <- forkIO tgrp2 $ feedChannel bChan (sample n bWt fs)
            (_, g) <- forkIO tgrp2 $ feedChannel gChan (sample n gWt fs)
            (_, r) <- forkIO tgrp2 $ feedChannel rChan (sample n rWt fs)
            
            bVal <- fmap (ramp 65535) . result =<< b
            gVal <- fmap (ramp 65535) . result =<< g
            rVal <- fmap (ramp 65535) . result =<< r
            
            setRGB16 v (Addr16 2) rVal gVal bVal
            
            wait tgrp2
