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
    , costNormalized'_toAdd
    , gradientDescent
    , gradientDescentStep
    , getAllTrainingSetsOfOneFeature
    , GradDescentInfo
    , Alpha
    , Epsilon
    , Cost
    , Theta
    , NormalizeConst
    ) where

      import Matrix
      import DataBuilder (NumContainer(..), Header)
      import System.IO

      type X = Matrix Double                                                   --Matrix of data, columns are features, rows - training sets
      type Y = Matrix Double                                                   --vector vertical of result
      type Theta = Matrix Double                                               --vector vertical of theta
      type AmountOfBasicFeatures = Int
      data XYContainer = XYContainer ([Header], X, Y, AmountOfBasicFeatures)   --holder of X and Y
      type TrainingSet = Matrix Double                                         --vector vertical of data of one single training set. One transposed row of X.
      type NormalizeConst = Double
      type Alpha = Double                                                      -- the rate of speed of gradient descent
      type Epsilon = Double                                                    -- covergence tale, very small
      type Cost = Double

      type GradDescentInfo = (Theta, Cost)

      instance Show XYContainer where
        show (XYContainer (headers, x, y, am)) = "---------X--------- \n" ++ show headers ++ "\n" ++ show x ++ "\n" ++ "---------Y--------- \n" ++ show y
                                                    ++ "\n" ++ "Amount of basic features: " ++ show am

      getXYContainer :: Int -> NumContainer -> XYContainer    --indexing from 1
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
                                                    vectorOfOnes = vector (replicate (getHeight $ mx) 1) 0
                                                    unJust = \(Just a) -> a



      cost :: Theta -> X -> Y -> Double
      cost theta mx my = (addLinesAndGetDouble . fmap (^2) $ (vectorOfEvaluatedHypothesis) - (transposeM $ my)) / (2*(fromIntegral . length $ unpackedmx))
        where
          unpackedmx = unpackM . transposeM $ mx    --transposed list of lists of matrix X
          addLinesAndGetDouble = (\(Matrix [[x]]) -> x) . zipWithLines (+)
          forEachListEvalHypothesis = (\xs -> [evalHypothesis (packM [xs]) theta])
          vectorOfEvaluatedHypothesis = packM $ map forEachListEvalHypothesis unpackedmx


      costNormalized :: XYContainer -> Theta -> NormalizeConst -> Double    --dla wersji I
      costNormalized (XYContainer (s, x, y, n)) theta normconst = (cost theta x y) + (normconst*(sumThets (n+1) (2*n) theta)) + (normconst*normconst*(sumThets (2*n+1) (3*n) theta))
        where
          sumThets x y thetaa
            | y - x == 0 = abs (getElementByInd 1 x thetaa)
            | y - x == 1 = (abs (getElementByInd 1 x thetaa)) + (abs (getElementByInd 1 y thetaa))
            | otherwise = (abs (getElementByInd 1 x thetaa)) + (sumThets (x + 1) y thetaa)




      cost' :: Int -> Theta -> X -> Y -> Double
      cost' varOfDer theta mx my = ((getElement $ ((vectorOfEvaluatedHypothesis) - (transposeM $ my))*(getAllTrainingSetsOfOneFeature varOfDer mx)) / (fromIntegral . length $ unpackedmx))
                where
                  unpackedmx = unpackM . transposeM $ mx                                          --transposed list of lists of matrix X
                  getElement = (\(Matrix [[x]]) -> x) -- . zipWithLines (+)
                  forEachListEvalHypothesis = (\xs -> [evalHypothesis (packM [xs]) theta])
                  vectorOfEvaluatedHypothesis = packM $ map forEachListEvalHypothesis unpackedmx


      costNormalized'_toAdd :: Int -> AmountOfBasicFeatures -> NormalizeConst -> Double    --dla wersji I
      costNormalized'_toAdd varOfDer n normconst
        | varOfDer <=n && varOfDer > 0 = 0
        | varOfDer > n && varOfDer <= 2*n = normconst
        | varOfDer > 2*n && varOfDer <= 3*n = normconst*normconst
        | varOfDer == 3*n+1 = 0     --free term is in the last column
        | otherwise = error ("costNormalized' : the index of the variable you want to derivative by is bigger than the amount of features.")



      gradientDescentStep :: XYContainer -> Theta -> Alpha -> NormalizeConst -> Theta
      gradientDescentStep xyc theta alpha normconst = loop (getWidth . getX $ xyc)
        where
          loop 1 = Matrix ([[(getElementByInd 1 1 theta) - (alpha * (costeval' 1 xyc theta normconst))]])
          loop i = unJust ((Matrix ([[(getElementByInd 1 i theta) - (alpha * (costeval' i xyc theta normconst))]])) `conhor` (loop (i-1)))
          unJust = \(Just a) -> a
          costeval' varOfDer xyCon tht normc = (cost' varOfDer tht (getX xyCon) (getY xyCon)) + (costNormalized'_toAdd varOfDer (getAmountOfBasicFeatures xyCon) normc)

      gradientDescent :: XYContainer -> Alpha -> NormalizeConst -> Epsilon -> Int -> IO (GradDescentInfo)
      gradientDescent xyc alpha normconst epsilon maxiter = loop epsilon (thetaInit xyc 0.7) 0
        where
          loop eps tht i = do
            currentCost <- return $ costNormalized xyc tht normconst
            if abs(currentCost) < eps || i == maxiter
              then return (tht, currentCost)
              else
                print ("Current cost = " ++ (show currentCost)) >> print tht >>
                loop eps (gradientDescentStep xyc tht alpha normconst) (i+1)

      --getters

      getX :: XYContainer -> X
      getX (XYContainer (_, x, _, _)) = x

      getY :: XYContainer -> Y
      getY (XYContainer (_, _, y, _)) = y

      getHeaders :: XYContainer -> [Header]
      getHeaders (XYContainer (xs, _, _, _)) = xs

      getAmountOfBasicFeatures :: XYContainer -> AmountOfBasicFeatures
      getAmountOfBasicFeatures (XYContainer (_, _, _, am)) = am

      getLastCost :: GradDescentInfo -> Cost
      getLastCost (_, cost) = cost

      getLastTheta :: GradDescentInfo -> Theta
      getLastTheta (theta, _) = theta

--PRIVATE FUNCTIONS

      evalHypothesis :: TrainingSet -> Theta -> Double
      evalHypothesis x theta = head . head $ unpackM $ (transposeM theta) * x

      getTrainingSet :: Int -> X -> TrainingSet
      getTrainingSet n xs = getColumn n (transposeM xs)

      getAllTrainingSetsOfOneFeature :: Int -> X -> Matrix Double
      getAllTrainingSetsOfOneFeature i mx = getColumn i mx


      thetaInit :: XYContainer -> Double -> Theta
      thetaInit (XYContainer (xs, _, _, _)) initval = vector (replicate (length xs) initval) 0

      allBut_ :: Int -> [a] -> [a]
      allBut_ _ [] = []
      allBut_ 1 (x:xs) = allBut_ 0 xs
      allBut_ n (x:xs) = x:(allBut_ (n-1) xs)
