module Main where

import ColorOrgan
import Control.Monad
import qualified Data.Vector.Unboxed as U
import Spectrum

fftSize = 2^16
sampleRate = 44100
captureBufSize = 4096

main = do
    s <- newSpectrum fftSize sampleRate
    c <- newColorOrgan sampleRate captureBufSize
    
    forever $ do
        tickColorOrgan c s
        print . log =<< getMagnitude s 8000
