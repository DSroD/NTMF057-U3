{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bessel
import NumericalTools
import Utils
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Graphics.Plotly
import Graphics.Plotly.Lucid
import Lucid
import Lucid.Html5

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)


plotFunction :: (Num a) => (a -> Double) -> [a] -> [(a, Double)]
plotFunction func = map (\x -> (x, func x))

plotError :: IntParametrizedFunc  -> Double -> Int -> [Int] -> [(Int, Double)]
plotError f x param i = zip i (numericalError f x param i)

logLogScript :: Monad m => HtmlT m ()
logLogScript = script_ [src_ "loglog.js"] $ toHtml("" :: String)

main :: IO ()
main =
    let num_steps = 4096
        perfect_steps = 16384
        computed_bessel = plotFunction (besselJ0 num_steps) [0.0, 0.001 .. 11]
        roots = map (uncurry $ findRootOfFuncMiddlePointMethod (besselJ0 num_steps) 1e-12) [(2, 4), (5, 6), (8, 10)]
        rootsSecant = map (uncurry $ findRootOfFuncSecantMethod (besselJ0 num_steps) 2000 1e-7) [(2, 3), (5, 6), (8, 9)]
        errorTrace = Graphics.Plotly.line (aes & x .~ fst & y .~ snd) (plotError besselJ0 (head roots) perfect_steps [2^n | n <- [2..13]])
        
    in do
    toFile def "bessel.png" $ do
        layout_title .= "Bessel function (4096 integration steps)"
        setColors [opaque blue]
        plot (Graphics.Rendering.Chart.Easy.line "Bessel function J_0(k)" [computed_bessel])

    putStr "Bisection method: "
    print roots
    putStrLn "----------------------"
    putStr "Secant method: "
    print rootsSecant
        
    T.writeFile "errors.html" $ renderText $ doctypehtml_ $ do
        head_ $ do meta_ [charset_ "utf-8"]
                   logLogScript
                   plotlyCDN
        body_ $ toHtml $ plotly "errorPlotDiv" [errorTrace]  
                    & layout . title ?~ "J_0(0) error |N steps - 16384 steps|"
                    & layout . xaxis . _Just . axistype ?~ Log
                    & layout . yaxis . _Just . axistype ?~ Log
