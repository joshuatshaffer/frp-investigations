{-# LANGUAGE Arrows #-}

module Main (main) where

import FRP.Yampa
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)

type Pos = (Double,Double)
type Vel = (Double,Double)

integral2 = integral *** integral

add2 (x1,y1) (x2,y2) = (x1+x2, y1+y2)

gravityAt (x,y) = (x*s, y*s)
  where r2 = x*x + y*y
        s = -9.81 / (r2 * sqrt r2)

fallingBall :: Pos -> Vel -> SF () (Pos, Vel)
fallingBall p0 v0 = proc () -> do
  rec
    let g = gravityAt p
    v <- add2 v0 ^<< integral2 -< g
    p <- add2 p0 ^<< integral2 -< v
  returnA -< (p, v)

main :: IO ()
main = do
  reactimate (return ())
             (\_ -> return (1/60, Nothing))
             (\_ ((x,y), _) -> putStrLn (show x ++ " " ++ show y) >> return ((x*x + y*y) > 56.25))
             (fallingBall (5, 5) (-0.8328695890378056, 0.8328695890378056))
