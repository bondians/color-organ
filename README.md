Building
========

This is very much a work in progress.
You'll need the Haskell Platform and fftw3 installed in a pkgconfig-compatible way (homebrew works).

You'll also need the hs-vTree package from github.

General procedure:

    cabal update
    
    cd somewhere/hs-vTree
    cabal install
    
    cd somewhere/color-organ
    cabal install
    runhaskell Test.hs


You'll probably have to modify the "/dev/cu.usbmodem" part of Test.hs.


