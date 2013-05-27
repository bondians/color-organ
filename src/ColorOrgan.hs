{-# LANGUAGE ForeignFunctionInterface #-}
module ColorOrgan where

import Control.Applicative
import Control.Monad
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import Spectrum.Internal

newtype ColorOrgan = ColorOrgan (ForeignPtr ColorOrgan)

foreign import ccall "color_organ_init"
    color_organ_init :: CDouble -> CSize -> IO (Ptr ColorOrgan)

foreign import ccall "&color_organ_cleanup"
    color_organ_cleanup :: FunPtr (Ptr ColorOrgan -> IO ())

foreign import ccall "color_organ_tick"
    color_organ_tick :: Ptr ColorOrgan -> Ptr Spectrum -> IO CInt

newColorOrgan :: Double -> Int -> IO ColorOrgan
newColorOrgan sampleRate captureBufSize = do
    c <- color_organ_init (realToFrac sampleRate) (fromIntegral captureBufSize)
    when (c == nullPtr) $ fail "newColorOrgan: failed to open audio device"
    ColorOrgan <$> newForeignPtr color_organ_cleanup c

tickColorOrgan :: ColorOrgan -> Spectrum -> IO Int
tickColorOrgan (ColorOrgan fp) s = withForeignPtr fp $ \c ->
    fromIntegral <$>
        withSpectrum s (color_organ_tick c)

