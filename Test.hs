module Main where

import ColorOrgan
import Control.Monad
import Data.Complex
import qualified Data.Vector.Unboxed as U
import Spectrum
import VTree
import Channel
import Crossover

fftSize = 2^12
sampleRate = 44100
captureBufSize = 4096

smooth = log 1.8
tuningPeriod = 5

cutoff = 0.15
clip x = max 0 (min 1 ((x - cutoff) / (1 - cutoff)))
ramp k x = round (255 * (clip x ** k))

xOver = crossover 0 [(440, 20), (4500, 50), (15000, 500)] (sampleRate/2)
dot v1 v2 = U.sum (U.zipWith (*) v1 v2)
sample n fs wts = ChanSample n (dot (U.map magnitude fs) wts)

main = do
    s <- newSpectrum fftSize sampleRate
    c <- newColorOrgan sampleRate captureBufSize
    v <- openVTreeLed "/dev/cu.usbmodem12341" 3
    
    freqs <- getBinFrequencies s
    let [rWt, yWt, gWt, bWt] = map (tabulate freqs) xOver
    
    rChan <- newChannel smooth (round (sampleRate * tuningPeriod))
    yChan <- newChannel smooth (round (sampleRate * tuningPeriod))
    gChan <- newChannel smooth (round (sampleRate * tuningPeriod))
    bChan <- newChannel smooth (round (sampleRate * tuningPeriod))
    
    forever $ do
        n <- tickColorOrgan c s
        fs <- getSpectrum s
        
        set Blue   v . ramp 3.0 =<< feedChannel bChan (sample n fs bWt)
        set Green  v . ramp 2.5 =<< feedChannel gChan (sample n fs gWt)
        set Yellow v . ramp 1.5 =<< feedChannel yChan (sample n fs yWt)
        set Red    v . ramp 1.8 =<< feedChannel rChan (sample n fs rWt)
