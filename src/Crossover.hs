module Crossover where

import Control.Applicative
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Math.Spline
import Math.Spline.BSpline
import Math.Spline.Knots

band :: Double -> Double -> Double -> Double -> BSpline U.Vector Double
band a b c d = bSpline (fromList [(a,3),(b,2),(c,2),(d,3)]) (U.fromList [0,0.5,1,1,1,0.5,0])

crossover :: [(Double, Double)] -> [BSpline U.Vector Double]
crossover transitions =
    [ band (x-wX/2) (x+wX/2) (y-wY/2) (y+wY/2)
    | ((x, wX),(y, wY)) <- zip transitions (drop 1 transitions)
    ]

tabulate :: U.Vector Double -> BSpline U.Vector Double -> U.Vector Double
tabulate freqs f = U.map (evalNaturalBSpline f) freqs
