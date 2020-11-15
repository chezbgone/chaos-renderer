{-|
Module      : Interpolation
Description : 2D Monotonic Cubic Interpolation
Copyright   : (c) Jason Chen, 2020
License     : MIT
Maintainer  : chezbgone (at) gmail dot com
Stability   : experimental
-}
module Interpolation where

type Point = (Double, Double)
type SlopedPoint = (Double, Double, Double)
type ControlPoints = [SlopedPoint]

temp = undefined

slope :: Point -> Point -> Double
slope (x0, y0) (x1, y1) = (y1 - y0) / (x1 - x0)

secantSlopes :: [Point] -> [Double]
secantSlopes pts = zipWith slope pts (tail pts)

tangentSlope :: Double -> Double -> Double
tangentSlope d0 d1
  | d0 < 0 && 0 < d1    = 0
  | d1 < 0 && 0 < d0    = 0
  | abs d0 > 5 * abs d1 = 3 * d1 -- clip to prevent overshoot
  | abs d1 > 5 * abs d0 = 3 * d0
  | otherwise           = (d0 + d1) / 2

tangentSlopes :: [Double] -> [Double]
tangentSlopes secantslopes = head secantslopes :
                              zipWith tangentSlope secantslopes (tail secantslopes) ++
                              [last secantslopes]

-- https://en.wikipedia.org/wiki/Cubic_Hermite_spline
cubicHermite :: SlopedPoint -> SlopedPoint -> (Double -> Double)
cubicHermite (xk, yk, dk) (xk1, yk1, dk1) x =
    h00t * yk + h10t * (xk1 - xk) * dk + h01t * yk1 + h11t * (xk1 - xk) * dk1
        where t = (x - xk) / (xk1 - xk)
              h00t = (1 + 2*t) * (1 - t)^2
              h10t = t * (1 - t)^2
              h01t = t^2 * (3 - 2*t)
              h11t = t^2 * (t - 1)

slopedPoint :: Point -> Double -> SlopedPoint
slopedPoint (x, y) m = (x, y, m)

piecewiseMonotonicCurve :: [Point] -> ControlPoints
piecewiseMonotonicCurve points = zipWith slopedPoint points tangentslopes
    where secantslopes = secantSlopes points
          tangentslopes = tangentSlopes secantslopes

eval :: ControlPoints -> Double -> Double
eval [] _ = 0
eval [(_, y, _)] _ = y
eval (sp0@(x0, y0, _):rest@(sp1@(x1, _, _):_)) x
  | x <= x0   = y0
  | x < x1    = cubicHermite sp0 sp1 x
  | otherwise = eval rest x
