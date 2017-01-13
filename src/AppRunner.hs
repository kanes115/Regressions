{-
TODO:
  - przy pdoawaniu training setu do evaluowania trzeba dodać coś, żeby automatycznie dodawały się nowe featury (kwadraty i sześciany)
-}
module AppRunner
  (
     parseCSV
   , prepareFile
   , train
   , guessY
   , getPreparedFile
   , getBaseXYContainer
   , getFullyPreparedXYContainer
  ) where

    import Regressions
    import DataBuilder
    import Matrix (vector)

    type Filepath = String
    type ColumnsToDelete = [Int]   --indexing from 1
    type Cost = Double
    type YColumnIndex = Int
    type MaxIter = Int

    type GradDescConsts = (Alpha, [(NormalizeConst, VariableInd)], Epsilon, MaxIter)
    type DataAlteringInfo = (ColumnsToDelete, YColumnIndex)

    type File = (NumContainer, DataAlteringInfo)
    type PreparedFile = (XYContainer, XYContainer)   -- |XYContainer before scaling and before adding free term, XYContainer after full preparing

    newtype TrainInfo = TrainInfo (GradDescentInfo, PreparedFile)

    instance Show TrainInfo where
      show (TrainInfo ((tht, cost), (unScaledUnNewfeat, fullyDone))) = ("|--------------------------------TRAIN INFO----------------------------------| \n"
                                                                        ++ "Result theta: \n" ++ (show tht) ++ "\n"
                                                                        ++ "Cost: " ++ (show cost) ++ "\n \n"
                                                                        ++ "Unscaled data: \n" ++ show unScaledUnNewfeat ++ "\n"
                                                                        ++ "Data used to training: \n" ++ show fullyDone)
                                                                        ++ "\n |----------------------------------------------------------------------------| \n"


    guessY :: TrainingSet -> TrainInfo -> Double
    guessY trainingSet (TrainInfo (gradDescInfo, (xycUnscaled, xyc))) = evaluateInputData trainingSet (getLastTheta gradDescInfo) xycUnscaled xyc


    train :: PreparedFile -> GradDescConsts -> IO (TrainInfo)
    train (u, xyc) consts = do
      gradDescInfo <- gradientDescent xyc (getAlpha consts) (getEpsilon consts) (getMaxIter consts) (getNormalizeConstList consts)
      return $ (TrainInfo (gradDescInfo, (u, xyc)))



    prepareFile :: File -> IO (PreparedFile)
    prepareFile (numContainer, (columnsToD, yColumnInd)) = do
        xycon <- return $ getXYContainer (yColumnInd - (length columnsToD)) numContainer
        newfeat <- return $ createNewFeature 1 2 xycon
        scaled <- return $  scale newfeat
        freeterm <- return $ scaled --addFreeTerm
        return $ (newfeat, freeterm)


    parseCSV :: Filepath -> DataAlteringInfo -> IO (File)
    parseCSV rawData (columnsToD, yColumn) = do
        datacontainer <- (getData rawData ",")
        return $ (filterDataContainer columnsToD datacontainer, (columnsToD, yColumn - (length columnsToD)))


    makeTrainingSet :: [Double] -> TrainingSet
    makeTrainingSet = Regressions.makeTrainingSet




    --getters

    getAlpha :: GradDescConsts -> Alpha
    getAlpha (alp, _, _, _) = alp

    getNormalizeConstList :: GradDescConsts -> [(NormalizeConst, VariableInd)]
    getNormalizeConstList (_, norm, _, _) = norm

    getEpsilon :: GradDescConsts -> Epsilon
    getEpsilon (_, _, eps, _) = eps

    getMaxIter :: GradDescConsts -> MaxIter
    getMaxIter (_, _, _, m) = m

    getPreparedFile :: TrainInfo -> PreparedFile
    getPreparedFile (TrainInfo (_, p)) = p

    getBaseXYContainer :: PreparedFile -> XYContainer
    getBaseXYContainer (a, b) = a

    getFullyPreparedXYContainer :: PreparedFile -> XYContainer
    getFullyPreparedXYContainer (a, b) = b


    -- PRIVATE FUNCTIONS
