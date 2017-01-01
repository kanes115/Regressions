{-

We assume last column is the Y column. The result we're trying to predict.

TODO:
  - function factory for creating new features in different ways
-}

module Regressions
    ( createNewFeatures
    , scale
    , getXYContainer
    , evalHypothesis
    , getTrainingSet
    , getX
    , getY
    , getHeaders
    , thetaInit
    ) where

      import Matrix
      import DataBuilder (NumContainer(..), Header)

      type X = Matrix Double                            --Matrix of data
      type Y = Matrix Double                            --vector vertical of result
      type Theta = Matrix Double                        --vector vertical of theta
      data XYContainer = XYContainer ([Header], X, Y)   --holder of X and Y
      type TrainingSet = Matrix Double                  --vector vertical of data of one single training set. Transposed row of X.

      instance Show XYContainer where
        show (XYContainer (headers, x, y)) = "---------X--------- \n" ++ show headers ++ "\n" ++ show x ++ "\n" ++ "---------Y--------- \n" ++ show y

      getXYContainer :: Int -> NumContainer -> XYContainer
      getXYContainer yColumnIndex (NumContainer (s, m)) = XYContainer (allBut_ yColumnIndex s, deleteColumns [yColumnIndex] m, Matrix [toList $ getColumn yColumnIndex m])





      createNewFeatures :: XYContainer -> XYContainer
      createNewFeatures (XYContainer (s , mx, my)) = XYContainer (s ++ (map (++" squared") s) ++ (map (++ " to power 3") s),
                                                  unJust ((unJust withQuad) `conver` (fmap (^3) mx)),
                                                  my
                                                  )
                                                  where
                                                    withQuad = mx `conver` (fmap (^2) mx)
                                                    unJust = \(Just a) -> a

      scale :: XYContainer -> XYContainer
      scale (XYContainer (s, mx, my)) = XYContainer (s, scaleLines mx, scaleLines my)


      --getters

      getX :: XYContainer -> X
      getX (XYContainer (_, x, _)) = x

      getY :: XYContainer -> Y
      getY (XYContainer (_, _, y)) = y

      getHeaders :: XYContainer -> [Header]
      getHeaders (XYContainer (xs, _, _)) = xs


--PRIVATE FUNCTIONS

      evalHypothesis :: TrainingSet -> Theta -> Double
      evalHypothesis x theta = head . head $ unpackM $ (transposeM theta) * x

      getTrainingSet :: Int -> X -> TrainingSet
      getTrainingSet n xs = getColumn n (transposeM xs)


      thetaInit :: XYContainer -> Double -> Theta
      thetaInit (XYContainer (xs, _, _)) initval = vector (replicate (length xs) initval) 0

      allBut_ :: Int -> [a] -> [a]
      allBut_ _ [] = []
      allBut_ 1 (x:xs) = allBut_ 0 xs
      allBut_ n (x:xs) = x:(allBut_ (n-1) xs)
