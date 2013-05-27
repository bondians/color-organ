module Main where

import ColorOrgan
import Control.Monad
import qualified Data.Vector.Unboxed as U
import Spectrum
import VTree

fftSize = 256
sampleRate = 44100
captureBufSize = 4096

convert k = round . max 0 . min 255 . (k *) . subtract 1000

main = do
    s <- newSpectrum fftSize sampleRate
    c <- newColorOrgan sampleRate captureBufSize
    v <- openVTreeLed "/dev/cu.usbmodem12341" 3
    
    forever $ do
        tickColorOrgan c s
        set Red    v . convert 1e-1 =<< getMagnitude s 100
        set Yellow v . convert 1e-1 =<< getMagnitude s 440
        set Green  v . convert 1e-1 =<< getMagnitude s 1200
        set Blue   v . convert 1e-3 =<< getMagnitude s 8000
