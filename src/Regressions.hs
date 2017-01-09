{-
  Module Regressions gives the tools to perform gradient decent including adding new features, scaling data and fitting polynomial to data.

  You can't scale after adding free term.
-}

module Regressions
    ( scale
    , addFreeTerm
    , getXYContainer
    , getX
    , getY
    , getHeaders
    , getAmountOfBasicFeatures
    , gradientDescent
    , getLastTheta
    , getLastCost
    , makeTrainingSet
    , createNewFeature
    , createNewFeaturesFromListToTrainingSet
    , evaluateInputData
    , GradDescentInfo
    , Alpha
    , Epsilon
    , Cost
    , Theta
    , NormalizeConst
    , TrainingSet
    , XYContainer
    , VariableInd
    , NewFeature
    ) where

      import Matrix
      import DataBuilder (NumContainer(..), Header)

      import System.IO
      import Data.List

      -- | Matrix of data, columns are features, rows training sets
      type X = Matrix Double
      -- | Vertical vector of the result
      type Y = Matrix Double
      --Vertical vector of theta
      type Theta = Matrix Double

      -- | Specifies the amount of features that are real, not created by program
      type AmountOfBasicFeatures = Int
      -- | Index of free term column
      type FreeTermColumnInd = Maybe Int
      -- | holder of X matrix, its headers, Y vector, information
      -- | about amount of basic features and information about if free term column has been added to X matrix,
      -- | a list of new features (tuples with information to what power which variable has been raised to in order to create new feature)
      data XYContainer = XYContainer ([Header], X, Y, AmountOfBasicFeatures, FreeTermColumnInd, [NewFeature], Scaled)
      -- | Horizontal vector of data of one single training set
      type TrainingSet = Matrix Double
      -- | Const that is used to multiply one of thetas element and then add the product to a cost function
      type NormalizeConst = Double
      -- | Learn speed rate
      type Alpha = Double
      -- | Convergence condition. Specifies how small should the difference between next costs be to stop gradient descent.
      type Epsilon = Double
      -- | Cost of polynomial fitting
      type Cost = Double
      -- | Index of a variable
      type VariableInd = Int
      -- | Number to which we take a number to
      type Power = Int
      -- | Maximum of iteration that gradient descent can perform
      type MaxIter = Int
      -- | Tuple with information on an added feature to data, in detail: about a power
      -- | to which variable on index VariableInd has been raised in order to create new feature
      type NewFeature = (Power, VariableInd)  -- freeterm nie jest newfeaturem

      type Scaled = Bool

      -- | Holds information about training. As a tuple: (Produced Theta, Last Cost)
      type GradDescentInfo = (Theta, Cost)

      -- * Instances
      instance Show XYContainer where
        show (XYContainer (headers, x, y, am, _, _, _)) = "---------X--------- \n" ++ show headers ++ "\n" ++ show x ++ "\n" ++ "---------Y--------- \n" ++ show y
                                                    ++ "\n" ++ "Amount of basic features: " ++ show am




      -- * FUNCTIONS INDEX
      -- | Takes an y column index, NumContainer and produces XYContainer - holder of X matrix, its headers, Y vector, information
      -- | about amount of basic features and information about if free term column has been added to X matrix. Indexing from 1
      getXYContainer :: Int -> NumContainer -> XYContainer
      -- | Scales data in XYContainer. For each element: x' = (x - avg(column where x is)) / range_of_values(column where x is)
      scale :: XYContainer -> XYContainer
      -- | Adds a column of ones at the end of X.
      addFreeTerm :: XYContainer -> XYContainer
      -- | Creates a TrainingSet out of a list
      makeTrainingSet :: [Double] -> TrainingSet
      -- | Creates new features to X by taking VariableInd-th feature and raise it to the power Power
      createNewFeature :: VariableInd -> Power -> XYContainer -> XYContainer
      -- | Performs a gradient descent algorithm for data in XYContainer. Alpha is a learn speed rate, epsilon is the convergence
      -- | condition (gradient descent stops when the difference between current cost and next cost is lower than epsilon),
      -- | MaxIter is a maximum of iteration you allow gradient descent to go through, and a list of (NormalizeConst, VariableInd)
      -- | is a list pairs of normalize const and variable index - it adds to the cost function the sum of products of elements of each pair.
      gradientDescent :: XYContainer -> Alpha -> Epsilon -> MaxIter -> [(NormalizeConst, VariableInd)] -> IO (GradDescentInfo)
      -- | Evaluates input data (just basic features needed to pass), Param: input data, theta, baseContainer (before scaling), fullyDoneContainer (after adding all the staff)
      evaluateInputData :: TrainingSet -> Theta -> XYContainer -> XYContainer -> Double
      -- * getters
      getX :: XYContainer -> X
      getY :: XYContainer -> Y
      getHeaders :: XYContainer -> [Header]
      getAmountOfBasicFeatures :: XYContainer -> AmountOfBasicFeatures
      getFreeTermColumnInd :: XYContainer -> FreeTermColumnInd
      getNewFeatureList :: XYContainer -> [NewFeature]
      getLastCost :: GradDescentInfo -> Cost
      getLastTheta :: GradDescentInfo -> Theta


      --bodies:


      getXYContainer yColumnIndex (NumContainer (s, m)) = XYContainer (allBut_ yColumnIndex s, deleteColumns [yColumnIndex] m, packM [toList $ getColumn yColumnIndex m], (length s) - 1, Nothing, [], False)


      createNewFeature varInd power (XYContainer (s , mx, my, am, wf, newF, False)) =
                                      XYContainer (
                                                  s ++ [(takeElement varInd s) ++ " to power " ++ show power],
                                                  unJust (mx `conver` (fmap (^ power) columnToAlter)),
                                                  my,
                                                  am,
                                                  wf,
                                                  newF ++ [(power, varInd)],
                                                  False
                                                  )
                                                  where
                                                    columnToAlter = getColumn varInd mx
                                                    unJust = \(Just a) -> a
                                                    takeElement 1 (x:xs) = x
                                                    takeElement i (x:xs) = takeElement (i-1) xs
      createNewFeature varInd power (XYContainer (s , mx, my, am, wf, newF, True)) = error "Can't add new features after scaling!"



      scale (XYContainer (s, mx, my, am, Nothing, newF, False)) = XYContainer (s, scaleLines mx, scaleLines my, am, Nothing, newF, True)
      scale (XYContainer (s, mx, my, am, _, newF, True)) = error "Can't scale more than one time."
      scale (XYContainer (s, mx, my, am, _, newF, _)) = error "Can't scale after adding free term."


      addFreeTerm (XYContainer (s , mx, my, am, Nothing, newF, sc)) = XYContainer (s ++ ["Free term"],
                                                                        unJust (mx `conver` vectorOfOnes),
                                                                        my,
                                                                        am,
                                                                        Just $ (length s) + 1,
                                                                        newF ++ [(0, 2)],  --we add free term which is any number to power 0
                                                                        sc
                                                                        )
                                                  where
                                                    vectorOfOnes = vector (replicate (getHeight $ mx) 1) 0
                                                    unJust = \(Just a) -> a
      addFreeTerm (XYContainer (s , mx, my, am, _, _, _)) = error "Free term already has been added."



      gradientDescent xyc alpha epsilon maxiter ns = loop epsilon (thetaInit xyc 0.7) 0
        where
          loop eps tht i = do
              currentCost <- return $ costNormalized xyc tht ns
              newCost <- return $ costNormalized xyc (gradientDescentStep xyc tht alpha ns) ns
              if abs(currentCost - newCost) < eps || i == maxiter
                then return ((gradientDescentStep xyc tht alpha ns), currentCost)
                else
                  print ("Current cost = " ++ (show currentCost)) >>
                  loop eps (gradientDescentStep xyc tht alpha ns) (i+1)




--Functions to cope with evaluating test data

    -- | Creates new features to the training set as specified in a list of info about new features. XYContainer is a base
    -- | container - before scaling and adding free term to it
      createNewFeaturesFromListToTrainingSet :: [NewFeature] -> TrainingSet -> TrainingSet
      createNewFeaturesFromListToTrainingSet nf trSet = unJust $ trSet `conver` (loop nf)
          where
            loop [] = trSet
            loop [(power, varInd)] = (fmap (^ power) columnToAlter varInd)
            loop ((power, varInd):nf) = unJust ( (fmap (^ power) columnToAlter varInd) `conver` (loop nf) )
            columnToAlter var = getColumn var trSet



      evaluateInputData inputData tht baseContainer fullyDone = case (getWidth withNewFeatures) == (getHeight tht) of
          True -> getElementByInd (1, 1) $ (scaleTrainingSet baseContainer withNewFeatures) * tht
          False -> getElementByInd (1, 1) $ (insertColumnAfter ((unJust . getFreeTermColumnInd $ fullyDone) - 1) 1 . scaleTrainingSet baseContainer $ withNewFeatures) * tht
          where
            withNewFeatures = createNewFeaturesFromListToTrainingSet (getNewFeatureList baseContainer) inputData
            insertColumnAfter i val m = unJust (m `conver` (packM [[val]]))




      scaleTrainingSet ::  XYContainer -> TrainingSet -> TrainingSet
      scaleTrainingSet baseContainer trainSet = zipWithM (+) (zipWithM (*) rangeMatrix trainSet) avgMatrix
        where
          avgMatrix = getAvgColumnsMatrix . getX $ (baseContainer)
          rangeMatrix = getRangeColumnsMatrix . getX $ (baseContainer)

-----------------------------------------

      makeTrainingSet xs = vector xs 1

      --getters


      getX (XYContainer (_, x, _, _, _, _, _)) = x


      getY (XYContainer (_, _, y, _, _, _, _)) = y


      getHeaders (XYContainer (xs, _, _, _, _, _, _)) = xs


      getAmountOfBasicFeatures (XYContainer (_, _, _, am, _, _, _)) = am

      getFreeTermColumnInd (XYContainer (_, _, _, _, wf, _, _)) = wf

      getNewFeatureList (XYContainer (_, _, _, _, _, newF, _)) = newF

      isScaled (XYContainer (_, _, _, _, _, _, sc)) = sc

      getLastCost (_, cost) = cost


      getLastTheta (theta, _) = theta








      --PRIVATE FUNCTIONS

      gradientDescentStep :: XYContainer -> Theta -> Alpha -> [(NormalizeConst, VariableInd)] -> Theta
      gradientDescentStep xyc theta alpha ns = loop (getWidth . getX $ xyc)
        where
          loop 1 = packM ([[(getElementByInd (1, 1) theta) - (alpha * (costeval' 1 xyc theta ns))]])
          loop i = unJust ((packM ([[(getElementByInd (1, i) theta) - (alpha * (costeval' i xyc theta ns))]])) `conhor` (loop (i-1)))
          unJust = \(Just a) -> a
          costeval' varOfDer xyCon tht ns = (costNormalized' varOfDer tht xyCon ns)


      cost :: Theta -> XYContainer -> Double
      cost theta (XYContainer(_, mx, my, _, _, _, _)) = (addLinesAndGetDouble . fmap (^2) $ (vectorOfEvaluatedHypothesis) - (transposeM $ my)) / (2*(fromIntegral . length $ unpackedmx))
        where
          unpackedmx = unpackM . transposeM $ mx    --transposed list of lists of matrix X
          addLinesAndGetDouble = getElementByInd (1, 1) . zipWithLines (+)
          forEachListEvalHypothesis = (\xs -> [evalHypothesis (vector xs 1) theta])
          vectorOfEvaluatedHypothesis = packM $ map forEachListEvalHypothesis unpackedmx


      cost' :: Int -> Theta -> XYContainer -> Double
      cost' varOfDer theta (XYContainer (_, mx, my, _, _, _, _)) =
            ((getElement $ ((vectorOfEvaluatedHypothesis) - (transposeM $ my))*(getAllTrainingSetsOfOneFeature varOfDer mx)) / (fromIntegral . length $ unpackedmx))
                where
                  unpackedmx = unpackM . transposeM $ mx                                          --transposed list of lists of matrix X
                  getElement = getElementByInd (1, 1)
                  forEachListEvalHypothesis = (\xs -> [evalHypothesis (vector xs 1) theta])
                  vectorOfEvaluatedHypothesis = packM $ map forEachListEvalHypothesis unpackedmx



      costNormalized :: XYContainer -> Theta -> [(NormalizeConst, VariableInd)] -> Double   --counts normalized cost for normalize consts and linked to them variables specified in the third argument
      costNormalized xyc theta nvs = cost theta xyc + sumNorms nvs
        where
          sumNorms [] = 0
          sumNorms [n] = evalNorms n
          sumNorms (n:ns) = evalNorms n + sumNorms ns
          evalNorms (norm, var) = norm * (getElementByInd (1, var) theta)


      costNormalized' :: Int -> Theta -> XYContainer -> [(NormalizeConst, VariableInd)] -> Double
      costNormalized' varOfDer tht (XYContainer (s, x, y, basic, ext, newF, sc)) ns =
            (cost' varOfDer tht (XYContainer (s, x, y, basic, ext, newF, sc))) + sumNorms ns
              where
                sumNorms [] = 0
                sumNorms [nv] = getNorm nv
                sumNorms (nv:nvs) = getNorm nv + sumNorms nvs
                getNorm (norm, var) = norm


      evalHypothesis :: TrainingSet -> Theta -> Double
      evalHypothesis x theta = getElementByInd (1, 1) (theta * x)

      getTrainingSet :: Int -> X -> TrainingSet
      getTrainingSet = getRow

      getAllTrainingSetsOfOneFeature :: Int -> X -> Matrix Double
      getAllTrainingSetsOfOneFeature i mx = getColumn i mx


      thetaInit :: XYContainer -> Double -> Theta
      thetaInit (XYContainer (xs, _, _, _, _, _, _)) initval = vector (replicate (length xs) initval) 0

      allBut_ :: Int -> [a] -> [a]
      allBut_ _ [] = []
      allBut_ 1 (x:xs) = allBut_ 0 xs
      allBut_ n (x:xs) = x:(allBut_ (n-1) xs)


      avg :: (Num a, Real a, Fractional a) => [a] -> a
      avg xs = realToFrac (sum xs) / genericLength xs

      range :: (Num a, Ord a) => [a] -> a
      range xs = maximum xs - minimum xs

      unJust :: Maybe a -> a
      unJust (Just x) = x
      inJust Nothing = error " "
