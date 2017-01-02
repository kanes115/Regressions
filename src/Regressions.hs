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
    , getAmountOfBasicFeatures
    , thetaInit
    , cost
    , costNormalized
    , addFreeTerm
    , cost'
    , costNormalized'
    , gradientDescent
    ) where

      import Matrix
      import DataBuilder (NumContainer(..), Header)

      type X = Matrix Double                                                   --Matrix of data
      type Y = Matrix Double                                                   --vector vertical of result
      type Theta = Matrix Double                                               --vector vertical of theta
      type AmountOfBasicFeatures = Int
      data XYContainer = XYContainer ([Header], X, Y, AmountOfBasicFeatures)   --holder of X and Y
      type TrainingSet = Matrix Double                                         --vector vertical of data of one single training set. One transposed row of X.
      type NormalizeConst = Double
      type Alpha = Double                                                      -- the rate of speed of gradient descent
      type Epsilon = Double                                                    -- covergence tale, very small

      instance Show XYContainer where
        show (XYContainer (headers, x, y, am)) = "---------X--------- \n" ++ show headers ++ "\n" ++ show x ++ "\n" ++ "---------Y--------- \n" ++ show y
                                                    ++ "\n" ++ "Amount of basic features: " ++ show am

      getXYContainer :: Int -> NumContainer -> XYContainer
      getXYContainer yColumnIndex (NumContainer (s, m)) = XYContainer (allBut_ yColumnIndex s, deleteColumns [yColumnIndex] m, Matrix [toList $ getColumn yColumnIndex m], (length s) - 1)





      createNewFeatures :: XYContainer -> XYContainer
      createNewFeatures (XYContainer (s , mx, my, am)) = XYContainer (s ++ (map (++" squared") s) ++ (map (++ " to power 3") s),
                                                  (unJust ((unJust withQuad) `conver` (fmap (^3) mx))),
                                                  my,
                                                  am
                                                  )
                                                  where
                                                    withQuad = mx `conver` (fmap (^2) mx)
                                                    unJust = \(Just a) -> a

      scale :: XYContainer -> XYContainer
      scale (XYContainer (s, mx, my, am)) = XYContainer (s, scaleLines mx, scaleLines my, am)

      addFreeTerm :: XYContainer -> XYContainer
      addFreeTerm (XYContainer (s , mx, my, am)) = XYContainer (s ++ ["Free term"],
                                                  unJust (mx `conver` vectorOfOnes),
                                                  my,
                                                  am
                                                  )
                                                  where
                                                    vectorOfOnes = vector (replicate (snd . getSize $ mx) 1) 0
                                                    unJust = \(Just a) -> a



      cost :: Theta -> X -> Y -> Double
      cost theta mx my = (addLinesAndGetDouble . fmap (^2) $ (vectorOfEvaluatedHypothesis) - (transposeM $ my)) / (2*(fromIntegral . length $ unpackedmx))
        where
          unpackedmx = unpackM . transposeM $ mx    --transposed list of lists of matrix X
          addLinesAndGetDouble = (\(Matrix [[x]]) -> x) . zipWithLines (+)
          forEachListEvalHypothesis = (\xs -> [evalHypothesis (packM [xs]) theta])
          vectorOfEvaluatedHypothesis = packM $ map forEachListEvalHypothesis unpackedmx


      costNormalized :: XYContainer -> Theta -> NormalizeConst -> Double    --dla wersji createNewFeatures (czyli z twoerzeniem po jednym kwadracie i po jednym szescianie)
      costNormalized (XYContainer (s, x, y, n)) theta normconst = (cost theta x y) + (normconst*(sumThets (n+1) (2*n) theta)) + (normconst*normconst*(sumThets (2*n+1) (3*n) theta))
        where
          sumThets x y thetaa = case  y - x == 1 of
            True -> (getElementByInd 1 x thetaa) + (getElementByInd 1 y thetaa)
            False -> (getElementByInd 1 x thetaa) + (sumThets (x + 1) y thetaa)




      cost' :: Int -> Theta -> X -> Y -> Double
      cost' varOfDer theta mx my = ((addLinesAndGetDouble $ ((vectorOfEvaluatedHypothesis) - (transposeM $ my))) / (fromIntegral . length $ unpackedmx)) * (getElementByInd 1 varOfDer theta)
                where
                  unpackedmx = unpackM . transposeM $ mx                                          --transposed list of lists of matrix X
                  addLinesAndGetDouble = (\(Matrix [[x]]) -> x) . zipWithLines (+)
                  forEachListEvalHypothesis = (\xs -> [evalHypothesis (packM [xs]) theta])
                  vectorOfEvaluatedHypothesis = packM $ map forEachListEvalHypothesis unpackedmx


      costNormalized' :: Int -> XYContainer -> Theta -> NormalizeConst -> Double    --dla wersji createNewFeatures (czyli z twoerzeniem po jednym kwadracie i po jednym szescianie)
      costNormalized' varOfDer (XYContainer (s, x, y, n)) theta normconst
        | varOfDer <=n = cost' varOfDer theta x y
        | varOfDer > n && varOfDer <= 2*n = (cost' varOfDer theta x y) + normconst
        | varOfDer > 2*n && varOfDer <= 3*n = (cost' varOfDer theta x y) + normconst*normconst



      gradientDescentStep :: XYContainer -> Theta -> Alpha -> NormalizeConst-> Theta
      gradientDescentStep xyc theta alpha normconst = loop (snd . getSize . getX $ xyc)
        where
          loop 1 = Matrix ([[(getElementByInd 1 1 theta) - (alpha * (costNormalized' 1 xyc theta normconst))]])
          loop i = unJust ((Matrix ([[(getElementByInd 1 i theta) - (alpha * (costNormalized' i xyc theta normconst))]])) `conhor` (loop (i-1)))
          unJust = \(Just a) -> a

      gradientDescent :: XYContainer -> Alpha -> NormalizeConst -> Epsilon -> Theta
      gradientDescent xyc alpha normconst epsilon = loop epsilon (thetaInit xyc 0.7)
        where
          loop eps tht
            | (costNormalized xyc tht normconst) < eps = tht
            | otherwise = loop eps (gradientDescentStep xyc tht alpha normconst)

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
