module AppRunner
  (train
  ) where

    import Regressions
    import DataBuilder

    type RunVersion = Int
    type Filepath = String
    type ColumnsToDelete = [Int]   --indexing from 1
    type TestTrainRatio = Double
    type Cost = Double
    type YColunIndex = Int
    type MaxIter = Int

    type GradDescConsts = (Alpha, NormalizeConst, Epsilon, MaxIter)

    newtype TrainInfo = TrainInfo (GradDescentInfo)

    instance Show TrainInfo where
      show (TrainInfo (tht, cost)) = ("Result theta: \n" ++ (show tht) ++ "\n" ++ "Cost: " ++ (show cost))


    train :: Filepath -> ColumnsToDelete -> YColunIndex -> RunVersion -> TestTrainRatio -> GradDescConsts -> IO (TrainInfo)
    train rawData columnsToD yColumnInd ver ratio consts
      | ver == 1 = do
        datacon <- (getData rawData ",")
        numcon <- (return $ (filterDataContainer columnsToD datacon))
        xycon <- return $ getXYContainer (yColumnInd - (length columnsToD)) numcon
        newfeat <- return $ createNewFeatures xycon
        scaled <- return $ scale newfeat   --scale przed newfeat
        print scaled
        freeterm <- return $ addFreeTerm newfeat
        gradDescInf <- (gradientDescent freeterm (getAlpha consts) (getNormalizeConst consts) (getEpsilon consts) (getMaxIter consts))
        return $ (TrainInfo gradDescInf)
      | otherwise = error ("train: Unknown version.")





    --getters

    getAlpha :: GradDescConsts -> Alpha
    getAlpha (alp, _, _, _) = alp

    getNormalizeConst :: GradDescConsts -> NormalizeConst
    getNormalizeConst (_, norm, _, _) = norm

    getEpsilon :: GradDescConsts -> Epsilon
    getEpsilon (_, _, eps, _) = eps

    getMaxIter :: GradDescConsts -> MaxIter
    getMaxIter (_, _, _, m) = m
