{-# LANGUAGE ForeignFunctionInterface #-}
module Spectrum.Internal where

import Data.Int
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr

data Spectrum = Spectrum !Int !(ForeignPtr Spectrum)

withSpectrum :: Spectrum -> (Ptr Spectrum -> IO a) -> IO a
withSpectrum (Spectrum _ fp) = withForeignPtr fp


foreign import ccall unsafe "spectrum_init"
    spectrum_init :: CSize -> CDouble -> IO (Ptr Spectrum)
foreign import ccall unsafe "&spectrum_cleanup"
    spectrum_cleanup :: FunPtr (Ptr Spectrum -> IO ())


foreign import ccall unsafe "spectrum_bin" 
    spectrum_bin :: Ptr Spectrum -> CDouble -> IO CInt
foreign import ccall unsafe "spectrum_freq" 
    spectrum_freq :: Ptr Spectrum -> CInt -> IO CDouble

foreign import ccall unsafe "spectrum_feed" 
    spectrum_feed :: Ptr Spectrum -> CSize -> Ptr Int16 -> IO CSize

foreign import ccall unsafe "spectrum_get"
    spectrum_get :: Ptr Spectrum -> IO (Ptr Double)

foreign import ccall unsafe "spectrum_get_mag" 
    spectrum_get_mag :: Ptr Spectrum -> CDouble -> IO CDouble

