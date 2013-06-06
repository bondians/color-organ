{-# LANGUAGE BangPatterns #-}
module Main where

import ColorOrgan
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

fftSize = 2^9
window f = ST.fromList
    [ f (fromIntegral i) (fromIntegral fftSize - 1)
    | i <- [0 .. fftSize-1]
    ]
sampleRate = 48000
captureBufSize = 2^14

atk = 0.06
dcy = 0.5
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
    v <- openVTreeLed "/dev/cu.usbmodem12341" 3
    
    freqs <- getBinFrequencies s
    let [rWt, yWt, gWt, _, bWt] = map (tabulate freqs) xOver
    
    rChan <- newChannel atk dcy (round (sampleRate * tuningPeriod))
    yChan <- newChannel atk dcy (round (sampleRate * tuningPeriod))
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
            (_, y) <- forkIO tgrp2 $ feedChannel yChan (sample n yWt fs)
            (_, r) <- forkIO tgrp2 $ feedChannel rChan (sample n rWt fs)
            
            set Blue   v . ramp 80  =<< result =<< b
            set Green  v . ramp 255 =<< result =<< g
            set Yellow v . ramp 255 =<< result =<< y
            set Red    v . ramp 255 =<< result =<< r
            
            wait tgrp2
