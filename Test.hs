module Main where

import ColorOrgan
import Control.Monad
import Data.Complex
import qualified Data.Vector.Unboxed as U
import Spectrum
import VTree
import Channel
import Crossover

fftSize = 2^9
sampleRate = 48000
captureBufSize = 4096

atk = 0.1
dcy = 1.3
tuningPeriod = 300

cutoff = 0.25
clip x = max 0 (min 1 ((x - cutoff) / (1 - cutoff)))
ramp k m x = round (m * (clip x ** k))

xOver = crossover [(15, 5), (250, 80), (800, 250), (3200, 250), (12000, 100), (15000, 1000)]
dot v1 v2 = U.sum (U.zipWith (*) v1 v2)
sample n fs wts = ChanSample n (dot (U.map magnitude fs) wts)

main = do
    s <- newSpectrum fftSize sampleRate
    c <- newColorOrgan sampleRate captureBufSize
    v <- openVTreeLed "/dev/cu.usbmodem12341" 3
    
    freqs <- getBinFrequencies s
    let [rWt, yWt, gWt, _, bWt] = map (tabulate freqs) xOver
    
    rChan <- newChannel atk dcy (round (sampleRate * tuningPeriod))
    yChan <- newChannel atk dcy (round (sampleRate * tuningPeriod))
    gChan <- newChannel atk dcy (round (sampleRate * tuningPeriod))
    bChan <- newChannel atk dcy (round (sampleRate * tuningPeriod))
    
    forever $ do
        n <- tickColorOrgan c s
        fs <- getSpectrum s
        
        set Blue   v . ramp 3.0 80  =<< feedChannel bChan (sample n fs bWt)
        set Green  v . ramp 2.5 255 =<< feedChannel gChan (sample n fs gWt)
        set Yellow v . ramp 2.5 255 =<< feedChannel yChan (sample n fs yWt)
        set Red    v . ramp 1.7 255 =<< feedChannel rChan (sample n fs rWt)
