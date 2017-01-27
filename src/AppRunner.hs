{-
  A module running gradient descent using Regressions module.
-}
module AppRunner
  (
     parseCSV
   , prepareFile
   , train
  --  , guessY
   , getPreparedFile
   , getBaseXYContainer
   , getFullyPreparedXYContainer
  ) where

    import Regressions
    import DataBuilder
    import Matrix (vector, getWidth, isEmpty, getSize)
    import Control.Exception
    import MaybeResult
    import Exceptions

    type Filepath = String
    -- | A list of indexes of columns that are to be deleted
    type ColumnsToDelete = [Int]   --indexing from 1
    -- | Cost of a fitting of polynomial
    type Cost = Double
    type YColumnIndex = Int
    -- | Upper limit of iterations that algorithm can make
    type MaxIter = Int

    -- | Constants describing algorithm of gradient descent
    type GradDescConsts = (Alpha, [(NormalizeConst, VariableInd)], Epsilon, MaxIter)
    type DataAlteringInfo = (ColumnsToDelete, YColumnIndex)

    -- | File contains Numcontainer with data and info about how it was altered
    type File = (NumContainer, DataAlteringInfo)
    -- | XYContainer before scaling and before adding free term, XYContainer after full preparing
    type PreparedFile = (XYContainer, XYContainer)
    -- | Onformation about performed gradient descent training
    newtype TrainInfo = TrainInfo (GradDescentInfo, PreparedFile)

    instance Show TrainInfo where
      show (TrainInfo ((tht, cost), (unScaledUnNewfeat, fullyDone))) = ("|--------------------------------TRAIN INFO----------------------------------| \n"
                                                                        ++ "Result theta: \n" ++ (show tht) ++ "\n"
                                                                        ++ "Cost: " ++ (show cost) ++ "\n \n"
                                                                        ++ "\n |----------------------------------------------------------------------------| \n")


    -- guessY :: TrainingSet -> TrainInfo -> Double
    -- guessY trainingSet (TrainInfo (gradDescInfo, (xycUnscaled, xyc))) = evaluateInputData trainingSet (getLastTheta gradDescInfo) xycUnscaled xyc


    train :: PreparedFile -> GradDescConsts -> IO (TrainInfo)
    train (u, xyc) consts = (do
      if (getSize . getX $ xyc) == (0, 0)
        then throwE "Cannot train file that has empty X matrix."
        else do
          if (any (\n -> n > amountOfVars) (map snd (getNormalizeConstList consts)))
            then throwE "\n Normalize consts specified incorrectly."
            else do
              gradDescInfo <- gradientDescent xyc (getAlpha consts) (getEpsilon consts) (getMaxIter consts) (getNormalizeConstList consts)
              return $ (TrainInfo (gradDescInfo, (u, xyc))))
      `catch` trainHandler
      where
        amountOfVars = getWidth . getX $ xyc



    prepareFile :: File -> IO (PreparedFile)    -- TODO dać możliwość definiowania, co robimy ze zbiorem treningowym programowi
    prepareFile (numContainer, (columnsToD, yColumnInd)) = (do
        xycon <- return $ getXYContainer (yColumnInd - (length columnsToD)) numContainer
        newfeat <- return $  (JustRes xycon) --createNewFeature 1 2
        if (isError newfeat)
          then do
            throwE (getError newfeat)
          else do
            scaled <- return $  scale (getResult newfeat)
            if (isError scaled)
              then throwE (getError scaled)
              else do
                freeterm <- return $ addFreeTerm (getResult scaled)
                if (isError freeterm)
                  then throwE (getError freeterm)
                  else do
                    return $ (getResult newfeat, getResult freeterm))
        `catch` filePreparerHandler


    parseCSV :: Filepath -> DataAlteringInfo -> IO (File)
    parseCSV rawData (columnsToD, yColumn) = (do
        datacontainer <- (getData rawData ",")
        if (yColumn > (columnsAmount datacontainer)) || (any (\n -> n > (columnsAmount datacontainer)) columnsToD)
          then throwE "\n Y column index or columns to delete out of boundaries."
          else do
            if (isEmpty . getMatrixFromNumContainer $ (filterDataContainer columnsToD datacontainer))
              then throwE "\n No data left after parsing."
              else do
                return $ (filterDataContainer columnsToD datacontainer, (columnsToD, yColumn - (length columnsToD))))
        `catch` fileParserHandler
      where
        columnsAmount = getWidth . getMatrixFromDataContainer




    makeTrainingSet :: [Double] -> TrainingSet
    makeTrainingSet = Regressions.makeTrainingSet



    -- *getters

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

    -- handlers
    filePreparerHandler :: IOError -> IO (PreparedFile)
    filePreparerHandler e =
      throwE ("\n" ++ show e ++ " \n Error while preapring file.")

    fileParserHandler :: IOError -> IO (File)
    fileParserHandler e =
      throwE ("\n" ++ show e ++ " \n Error while parsing data.")

    trainHandler :: IOError -> IO (TrainInfo)
    trainHandler e =
      throwE ("\n" ++ show e ++ " \n Error while training.")
