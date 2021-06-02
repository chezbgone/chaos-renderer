{-|
Module      : Mandlebrot
Description : 
Copyright   : (c) Jason Chen, 2021
License     : MIT
Maintainer  : chezbgone (at) gmail dot com
Stability   : experimental
-}
{-# LANGUAGE NamedFieldPuns #-}
module Mandlebrot where

import Data.Complex
import Data.List (iterate')
import Codec.Picture

import Interpolation


data Resolution = Resolution { width :: Int, height :: Int }

data Bounds = Bounds { top    :: Double
                     , bottom :: Double
                     , left   :: Double
                     , right  :: Double
                     }

data ComputationProps = ComputationProps
    { maxIterations :: Int
    , escapeRadius :: Double
    }

data RenderProps = RenderProps
    { resolution :: Resolution
    , bounds     :: Bounds
    }

data MandlebrotProps = MandlebrotProps
    { computationProps :: ComputationProps
    , renderProps      :: RenderProps
    }

magnitude2 :: RealFloat a => Complex a -> a
magnitude2 (x :+ y) = x*x + y*y

inMandlebrot :: ComputationProps -> Complex Double -> Bool
inMandlebrot ComputationProps{ maxIterations, escapeRadius } c =
    let f z = z^2 + c
        orbit = iterate' f 0
    in all (\p -> magnitude2 p < escapeRadius^2) $ take maxIterations orbit

bwPixel :: Bool -> PixelRGB8
bwPixel b = if b then PixelRGB8 0 0 0 else PixelRGB8 255 255 255

mandlebrotBW :: MandlebrotProps -> DynamicImage
mandlebrotBW mp =
    let cp = computationProps mp
        RenderProps{ resolution=Resolution{ width, height }
                   , bounds=Bounds{ top, bottom, left, right } } = renderProps mp
        dx = (right - left) / fromIntegral width
        dy = (top - bottom) / fromIntegral height
        complex x y = (left + fromIntegral x * dx) :+ (top - fromIntegral y * dy)
    in ImageRGB8 $ generateImage (\x y -> bwPixel $ inMandlebrot cp $ complex x y) width height



mandlebrotEsc :: ComputationProps -> Complex Double -> Int
mandlebrotEsc ComputationProps{ maxIterations, escapeRadius } c =
    let f z = z^2 + c
        orbit = iterate' f 0
    in length $ takeWhile (\p -> magnitude2 p < escapeRadius^2)
              $ take maxIterations orbit

xs = (255*) <$> [0, 0.16, 0.42, 0.6425, 0.8575]
rs = [0, 32, 237, 255, 0]
gs = [7, 107, 255, 170, 2]
bs = [100, 203, 255, 0, 0]

rf = interpolate $ piecewiseMonotonicCurve $ zip xs rs
gf = interpolate $ piecewiseMonotonicCurve $ zip xs gs
bf = interpolate $ piecewiseMonotonicCurve $ zip xs bs

intPixel :: Int -> PixelRGB8
intPixel esc = let esc' = fromIntegral esc
    in PixelRGB8 (round $ rf esc') (round $ gf esc') (round $ bf esc')

mandlebrotInt :: MandlebrotProps -> DynamicImage
mandlebrotInt mp =
    let cp = computationProps mp
        RenderProps{ resolution=Resolution{ width, height }
                   , bounds=Bounds{ top, bottom, left, right } } = renderProps mp
        dx = (right - left) / fromIntegral width
        dy = (top - bottom) / fromIntegral height
        complex x y = (left + fromIntegral x * dx) :+ (top - fromIntegral y * dy)
    in ImageRGB8 $ generateImage (\x y -> intPixel $ mandlebrotEsc cp $ complex x y) width height



