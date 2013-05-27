module Main where

import ColorOrgan
import Control.Monad
import qualified Data.Vector.Unboxed as U
import Spectrum
import VTree

fftSize = 2048
sampleRate = 44100
captureBufSize = 4096

convert k = round . max 0 . min 255 . subtract 20 . (k *)

main = do
    s <- newSpectrum fftSize sampleRate
    c <- newColorOrgan sampleRate captureBufSize
    v <- openVTreeLed "/dev/cu.usbmodem12341" 3
    
    forever $ do
        tickColorOrgan c s
        set Red    v . convert 4e-4 =<< getMagnitude s 220
        set Yellow v . convert 1e-4 =<< getMagnitude s 1200
        set Green  v . convert 1e-3 =<< getMagnitude s 5000
        set Blue   v . convert 3e-3 =<< getMagnitude s 16000
