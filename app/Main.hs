module Main where

import Bessel
import NumericalTools
import Utils
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

plotFunction :: (Num a) => (a -> Double) -> [a] -> [(a, Double)]
plotFunction func = map (\x -> (x, func x))

plotError :: IntParametrizedFunc  -> Double -> Int -> [Int] -> [(Int, Double)]
plotError f x param i = zip i (numericalError f x param i)

main :: IO ()
main =
    let num_steps = 4096
        perfect_steps = 16384
        computed_bessel = plotFunction (besselJ0 num_steps) [0.0, 0.001 .. 11]
        roots = map (uncurry $ findRootOfFuncMiddlePointMethod (besselJ0 num_steps) 1e-12) [(2, 4), (5, 6), (8, 10)]
        rootsSecant = map (uncurry $ findRootOfFuncSecantMethod (besselJ0 num_steps) 2000 1e-7) [(2, 3), (5, 6), (8, 9)]
    in do
    toFile def "bessel.png" $ do
        layout_title .= "Bessel function (4096 integration steps)"
        setColors [opaque blue]
        plot (line "Bessel function J_0(k)" [computed_bessel])

    putStr "Bisection method: "
    print roots
    putStrLn "----------------------"
    putStr "Secant method: "
    print rootsSecant
        