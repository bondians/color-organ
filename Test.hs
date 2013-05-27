module Main where

import ColorOrgan
import Control.Monad
import qualified Data.Vector.Unboxed as U
import Spectrum
import VTree
import Channel

fftSize = 2048
sampleRate = 44100
captureBufSize = 4096

smooth = log 3
tuningPeriod = 30

clip x = max 0 (min 1 ((x - cutoff) / (1 - cutoff)))
cutoff = 0.2
ramp k x = round (255 * (clip x ** k))

main = do
    s <- newSpectrum fftSize sampleRate
    c <- newColorOrgan sampleRate captureBufSize
    v <- openVTreeLed "/dev/cu.usbmodem12341" 3
    
    rChan <- newChannel smooth (round (sampleRate * tuningPeriod))
    yChan <- newChannel smooth (round (sampleRate * tuningPeriod))
    gChan <- newChannel smooth (round (sampleRate * tuningPeriod))
    bChan <- newChannel smooth (round (sampleRate * tuningPeriod))
    
    forever $ do
        n <- tickColorOrgan c s
        
        set Blue   v . ramp 2.0 =<< feedChannel bChan . ChanSample n =<< getMagnitude s 12000
        set Green  v . ramp 3.0 =<< feedChannel gChan . ChanSample n =<< getMagnitude s 4000
        set Yellow v . ramp 1.5 =<< feedChannel yChan . ChanSample n =<< getMagnitude s 800
        set Red    v . ramp 1.2 =<< feedChannel rChan . ChanSample n =<< getMagnitude s 200
