module Main where

import Bessel
import NumericalTools
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

plotFunction :: (Num a) => (a -> Double) -> [a] -> [(a, Double)]
plotFunction func = map (\x -> (x, func x))

main :: IO ()
main = 
    let num_steps = 4096
        computed_bessel = plotFunction (besselJ0 num_steps) [0.0, 0.001.. 11]
    in do
    toFile def "bessel.png" $ do
        layout_title .= "Bessel function (4096 integration steps)"
        setColors [opaque blue]
        plot (line "Bessel function J_0(k)" [computed_bessel])