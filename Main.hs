{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import Control.Concurrent (threadDelay)

fallingBall :: Double -> SF () (Double, Double)
fallingBall y0 = proc dt -> do
  a <- constant (-9.81)      -< dt
  v <- integral              -< a
  y <- (integral >>^ (+ y0)) -< v
  returnA -< (y, v)

main :: IO ()
main = do
  putStrLn "Hello World"
  reactimate (return ())
             (\_ -> threadDelay 100000 >> return (0.1, Nothing))
             (\ _ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel) >> return False)
             (fallingBall 10.0)
