module Spectrum
    ( Spectrum
    , newSpectrum
    
    , frequencyToBin
    , binToFrequency
    
    , feed
    , getSpectrum
    , getMagnitude
    
    , getBinFrequencies
    ) where

import Control.Applicative
import Data.Complex
import Data.Int
import qualified Data.Vector.Storable as ST
import qualified Data.Vector.Unboxed as U
import Foreign.ForeignPtr
import Foreign.Storable
import Spectrum.Internal

newSpectrum :: Int -> Double -> IO Spectrum
newSpectrum sz sampleRate = do
    s <- spectrum_init (fromIntegral sz) (realToFrac sampleRate)
    Spectrum sz <$> newForeignPtr spectrum_cleanup s

frequencyToBin :: Spectrum -> Double -> IO Int
frequencyToBin s freq = withSpectrum s $ \s' ->
    fromIntegral <$>
        spectrum_bin s' (realToFrac freq)

binToFrequency :: Spectrum -> Int -> IO Double
binToFrequency s bin = withSpectrum s $ \s' ->
    realToFrac <$>
        spectrum_freq s' (fromIntegral bin)

feed :: Spectrum -> ST.Vector Int16 -> IO Int
feed s vec = withSpectrum s $ \s' ->
    ST.unsafeWith vec $ \samples ->
        fromIntegral <$> 
            spectrum_feed s' (fromIntegral $ ST.length vec) samples

getSpectrum :: Spectrum -> IO (U.Vector (Complex Double))
getSpectrum s@(Spectrum sz _) = withSpectrum s $ \s' -> do
    sData   <- spectrum_get s'
    let n = sz `div` 2 + 1
    U.generateM n $ \i -> do
        a <- peekElemOff sData (2 * i)
        b <- peekElemOff sData (2 * i + 1)
        return (a :+ b)

getMagnitude :: Spectrum -> Double -> IO Double
getMagnitude s freq = withSpectrum s $ \s' ->
    realToFrac <$>
        spectrum_get_mag s' (realToFrac freq)

getBinFrequencies :: Spectrum -> IO (U.Vector Double)
getBinFrequencies s@(Spectrum n _) =
    U.generateM (n `div` 2 + 1) (binToFrequency s)
