module Utils(shiftInterval, Func, IntParametrizedFunc) where

type Func = Double -> Double
type IntParametrizedFunc = Int -> Double -> Double

shiftInterval :: (Num a) => [a] -> [(a, a)]
shiftInterval i = zip (take (length i - 1) i) (drop 1 i)