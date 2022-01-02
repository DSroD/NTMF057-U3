module NumericalTools (numericalError, numericalConvergenceSpeed) where

import Utils
-- |Integer parametrized function, vaule where to evaluate, best parameter value, parameters to evaluate -> numerical errors
numericalError :: IntParametrizedFunc -> Double -> Int -> [Int] -> [Double]
numericalError f x max_param =
    let max_f = f max_param x
    in map (\n -> f n x - max_f)

-- |Integer parametrized function, vaule where to evaluate, best parameter value, order, parameters to evaluate -> convergence speed
numericalConvergenceSpeed :: IntParametrizedFunc -> Double -> Int -> Int -> [Int] -> [Double]
numericalConvergenceSpeed f x max_param order =
    let max_f = f max_param x
    in map (\n -> (f (fst n) x - max_f) / abs ((fromIntegral . snd) n - max_f) ^^ order) . shiftInterval