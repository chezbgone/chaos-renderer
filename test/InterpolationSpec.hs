import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Interpolation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Interpolation tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(tested by QuickCheck)"
    [ QC.testProperty "derivative zero at local extrema" $
        \y0 y1 y2 ->
            (y0 > y1 && y1 < y2) || (y0 < y1 && y1 > y2) QC.==>
                tangentSlopes (secantSlopes [(0, y0), (1, y1), (2, y2)]) !! 1 == 0
    , QC.testProperty "curves do not overshoot" $
        withMaxSuccess 1000 $
        \y0 y1 y2 y3 x ->
            let points = [(-1, y0), (0, y1), (1, y2), (2, y3)]
                x' = snd $ properFraction $ abs x
                y = eval (piecewiseMonotonicCurve points) x'
            in y1 <= y && y <= y2 || y1 >= y && y >= y2
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "line segment" $ do
        let interpolate = eval $ piecewiseMonotonicCurve [(0, 0), (1, 1)]
        mapM_ (\x -> x @~? interpolate x) [ 0
                                          , 0.0009940338877736
                                          , 0.1518801527149218
                                          , 0.2921518901599145
                                          , 0.3403131217615025
                                          , 0.3933227099911301
                                          , 0.6772573405540895
                                          , 0.7972561837300758
                                          , 0.9261566790263968
                                          , 0.9930867051019727
                                          , 1 ]
    , testCase "constant function" $ do
        let interpolate = eval $ piecewiseMonotonicCurve
              [(-1, 2), (0, 0), (1, 0), (2, -6)]
        mapM_ (\x -> 0 @~? interpolate x) [ 0
                                          , 0.0091093502544188
                                          , 0.1603816952625086
                                          , 0.3572486465068517
                                          , 0.4004735695957994
                                          , 0.4045733520007163
                                          , 0.4764537765059914
                                          , 0.5371976992047602
                                          , 0.6825052986838982
                                          , 0.7322642954197155
                                          , 1 ]
    , testCase "curve with no overshoot" $ do
        let interpolate = eval $ piecewiseMonotonicCurve
              [(0, 0), (1, 0.5), (2, 2)]
        mapM_ (interpolateBound interpolate)
              [ (0, 0, 0)
              , (0.01, 0, 0.5)
              , (0.99, 0, 0.5)
              , (1, 0.5, 0.5)
              , (1.01, 0.5, 2)
              , (1.99, 0.5, 2)
              , (2, 2, 2)]
    , testCase "curve originally with overshoot below" $ do
        let interpolate = eval $ piecewiseMonotonicCurve
              [(0, 0), (1, 0.2), (2, 3)]
        mapM_ (interpolateBound interpolate)
              [ (0, 0, 0)
              , (0.01, 0, 0.2)
              , (0.99, 0, 0.2)
              , (1, 0.2, 0.2)
              , (1.01, 0.2, 3)
              , (1.99, 0.2, 3)
              , (2, 3, 3)]
    , testCase "curve originally with overshoot above" $ do
        let interpolate = eval $ piecewiseMonotonicCurve
              [(0, 3), (1, 2.8), (2, 0)]
        mapM_ (interpolateBound interpolate)
              [ (0, 3, 3)
              , (0.01, 2.8, 3)
              , (0.99, 2.8, 3)
              , (1, 2.8, 2.8)
              , (1.01, 0, 2.8)
              , (1.99, 0, 2.8)
              , (2, 0, 0)]
    , testCase "zig zag" $ do
        let interpolate = eval $ piecewiseMonotonicCurve
              [(-1, 1), (0, 0), (1, 1), (2, 0)]
        mapM_ (interpolateBound interpolate)
              [ (-1, 1, 1)
              , (-0.1, 0, 1)
              , (0, 0, 0)
              , (0.05, 0, 1)
              , (0.1, 0, 1)
              , (0.9, 0, 1)
              , (0.95, 0, 1)
              , (1, 1, 1)
              , (1.05, 0, 1)
              , (1.1, 0, 1)
              , (2, 0, 0)]
    ]

interpolateBound :: (Double -> Double) -> (Double, Double, Double) -> Assertion
interpolateBound interpolate (x, min, max) =
    assertBool ("  interpolation overshot at " ++ show x ++ "\n  " ++
            show actual ++ " not in [" ++ show min ++ ", "++ show max ++ "]")
        (min <= actual && actual <= max)
    where actual = interpolate x

-- approximately equals
infix 1 @~?
(@~?) :: HasCallStack => Double -> Double -> Assertion
expected @~? actual =
    assertBool msg (actual - expected <= 1)
    where msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual
