module Main where

import Codec.Picture
import Mandlebrot

mandlebrotProps = MandlebrotProps
                    ComputationProps{ maxIterations = 256, escapeRadius = 2 }
                    RenderProps{ resolution=Resolution{ width = 9000, height = 6000 }
                               , bounds=Bounds{ top = 1, bottom= -1, left = -2, right = 1 }
                               }

mandlebrotProps' = MandlebrotProps
                    ComputationProps{ maxIterations = 256, escapeRadius = 2 }
                    RenderProps{ resolution=Resolution{ width = 3000, height = 2000 }
                               , bounds=Bounds{ top = 1, bottom= -1, left = -2, right = 1 }
                               }

main :: IO ()
main = do
    putStrLn "generating Mandlebrot set"
    savePngImage "mandlebrot_int.png" $ mandlebrotInt mandlebrotProps
    putStrLn "done generating"
