module Bessel (besselJ0, IntParametrizedFunc) where
    
import Utils

integrate :: [Double] -> Func -> Double
integrate xs f = sum [(f x + f  x') * (x' - x) / 2 | (x, x') <- shiftInterval xs]

-- | Bessel function of the first kind, parameter sets number of interation steps
besselJ0 :: IntParametrizedFunc
besselJ0 num_steps z = 
    let stepSize = 2 * pi / fromIntegral num_steps
    in 1 / (2 * pi) * integrate [-pi, stepSize - pi .. pi + stepSize] (cos . (*) z . cos)