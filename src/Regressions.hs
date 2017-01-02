{-

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

      type X = Matrix Double                                                   --Matrix of data
      type Y = Matrix Double                                                   --vector vertical of result
      type Theta = Matrix Double                                               --vector vertical of theta
      type AmountOfBasicFeatures = Int
      data XYContainer = XYContainer ([Header], X, Y, AmountOfBasicFeatures)   --holder of X and Y
      type TrainingSet = Matrix Double                                         --vector vertical of data of one single training set. One transposed row of X.

      instance Show XYContainer where
        show (XYContainer (headers, x, y, am)) = "---------X--------- \n" ++ show headers ++ "\n" ++ show x ++ "\n" ++ "---------Y--------- \n" ++ show y
                                                    ++ "\n" ++ "Amount of basic features: " ++ show am

      getXYContainer :: Int -> NumContainer -> XYContainer
      getXYContainer yColumnIndex (NumContainer (s, m)) = XYContainer (allBut_ yColumnIndex s, deleteColumns [yColumnIndex] m, Matrix [toList $ getColumn yColumnIndex m], length s)





      createNewFeatures :: XYContainer -> XYContainer
      createNewFeatures (XYContainer (s , mx, my, am)) = XYContainer (s ++ (map (++" squared") s) ++ (map (++ " to power 3") s),
                                                  unJust ((unJust withQuad) `conver` (fmap (^3) mx)),
                                                  my,
                                                  am
                                                  )
                                                  where
                                                    withQuad = mx `conver` (fmap (^2) mx)
                                                    unJust = \(Just a) -> a

      scale :: XYContainer -> XYContainer
      scale (XYContainer (s, mx, my, am)) = XYContainer (s, scaleLines mx, scaleLines my, am)


      cost :: Theta -> X -> Y -> Double -> Double
      cost theta mx my normConst = addLinesAndGetDouble . fmap (^2) $ (vectorOfEvaluatedHypothesis) - (transposeM $ my)
        where
          unpackedmx = unpackM . transposeM $ mx    --transposed list of lists of matrix X
          addLinesAndGetDouble = (\(Matrix [[x]]) -> x) . zipWithLines (+)
          forEachListEvalHypothesis = (\xs -> [evalHypothesis (packM [xs]) theta])
          vectorOfEvaluatedHypothesis = packM $ map forEachListEvalHypothesis unpackedmx

      

      --getters

      getX :: XYContainer -> X
      getX (XYContainer (_, x, _, _)) = x

      getY :: XYContainer -> Y
      getY (XYContainer (_, _, y, _)) = y

      getHeaders :: XYContainer -> [Header]
      getHeaders (XYContainer (xs, _, _, _)) = xs

      getAmountOfBasicFeatures :: XYContainer -> AmountOfBasicFeatures
      getAmountOfBasicFeatures (XYContainer (_, _, _, am)) = am

--PRIVATE FUNCTIONS

      evalHypothesis :: TrainingSet -> Theta -> Double
      evalHypothesis x theta = head . head $ unpackM $ (transposeM theta) * x

      getTrainingSet :: Int -> X -> TrainingSet
      getTrainingSet n xs = getColumn n (transposeM xs)


      thetaInit :: XYContainer -> Double -> Theta
      thetaInit (XYContainer (xs, _, _, _)) initval = vector (replicate (length xs) initval) 0

      allBut_ :: Int -> [a] -> [a]
      allBut_ _ [] = []
      allBut_ 1 (x:xs) = allBut_ 0 xs
      allBut_ n (x:xs) = x:(allBut_ (n-1) xs)
