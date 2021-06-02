{-|
Module      : Julia
Description : Computing Julia sets
Copyright   : (c) Jason Chen, 2021
License     : MIT
Maintainer  : chezbgone (at) gmail dot com
Stability   : experimental
-}
{-# LANGUAGE NamedFieldPuns #-}
module Julia where

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

data JuliaProps = JuliaProps
    { computationProps :: ComputationProps
    , renderProps      :: RenderProps
    }


escapeTime :: (Complex Double -> Complex Double) -- f
           -> ComputationProps
           -> Complex Double -- init
           -> Int
escapeTime f ComputationProps{ maxIterations, escapeRadius } init =
    length $ takeWhile (\z -> magnitude z < escapeRadius)
           $ take maxIterations
           $ iterate' f init

escapePixel :: Int -> PixelRGB8
escapePixel = undefined

gridToComplex :: RenderProps -> Int -> Int -> Complex Double
gridToComplex RenderProps{ resolution, bounds } x y
  = (left + xStep * fromIntegral x) :+ (top + yStep * fromIntegral y)
      where Resolution {width, height} = resolution
            Bounds {top, bottom, left, right} = bounds
            xStep = (right - left) / fromIntegral width
            yStep = (top - bottom) / fromIntegral height

generateJulia :: JuliaProps
              -> (Complex Double -> Complex Double) -- f
              -> FilePath
              -> IO ()
generateJulia JuliaProps{ computationProps, renderProps }
              f filepath = writePng filepath image
    where image = generateImage compute width height
          Resolution{ width, height } = resolution renderProps
          compute :: Int -> Int -> PixelRGB8
          compute x y = escapePixel $ escapeTime f computationProps
                                    $ gridToComplex renderProps x y
