module NumericalTools (numericalError, numericalConvergenceSpeed, findRootOfFuncMiddlePointMethod, findRootOfFuncSecantMethod) where
import Data.Maybe
import Utils
import Control.Applicative
import Control.Monad.Trans.State

-- |Integer parametrized function, vaule where to evaluate, best parameter value, parameters to evaluate -> numerical errors
numericalError :: IntParametrizedFunc -> Double -> Int -> [Int] -> [Double]
numericalError f x max_param =
    let max_f = f max_param x
    in map (\n -> abs (f n x - max_f))

-- |Integer parametrized function, vaule where to evaluate, best parameter value, order, parameters to evaluate -> convergence speed
numericalConvergenceSpeed :: IntParametrizedFunc -> Double -> Int -> Int -> [Int] -> [Double]
numericalConvergenceSpeed f x max_param order =
    let max_f = f max_param x
    in map (\n -> (f (fst n) x - max_f) / abs ((fromIntegral . snd) n - max_f) ^^ order) . shiftInterval

average2 :: (Fractional a) => a -> a -> a
average2 x y = (/) ((+) x y) 2

findRootOfFuncMiddlePointMethod :: Func -> Double -> Double -> Double -> Double
findRootOfFuncMiddlePointMethod f eps a b
    | abs (f x) < abs eps = x
    | otherwise =
        if f x * f a > 0
            then findRootOfFuncMiddlePointMethod f eps x b
            else findRootOfFuncMiddlePointMethod f eps a x
    where x = average2 a b

findRootOfFuncSecantMethod :: Func -> Int -> Double -> Double -> Double -> Double
findRootOfFuncSecantMethod f max_iter eps a b =
    if abs (x' - b) < abs eps then x' else evalState (findRootOfFuncSecantMethodS f max_iter eps b x') 0
    where
        x' = b - (f b * (b - a)) / (f b - f a)

-- Stateful implementation for maximal iteration count
findRootOfFuncSecantMethodS :: Func -> Int -> Double -> Double -> Double -> State Int Double
findRootOfFuncSecantMethodS f max_iter eps a b
    | abs (x' - b) < abs eps = return x'
    | otherwise = 
        do
            state <- get
            put (state + 1)
            if state == max_iter then return x' else findRootOfFuncSecantMethodS f max_iter eps b x'
    where
        x' = b - (f b * (b - a)) / (f b - f a)