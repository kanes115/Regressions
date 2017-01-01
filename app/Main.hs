module Main where

import Lib
import Matrix
import DataBuilder
import Regressions

main :: IO ()
main = do
    datacon <- (getData "examples/occupancy.csv" ",")
    numcon <- (return $ (filterDataContainer [1,2] datacon))
    xycon <- return $ getXYContainer 6 numcon
    newfeat <- return $ createNewFeatures xycon
    scaled <- return $ scale newfeat
    pp <- return $ evalHypothesis (getTrainingSet 54 . getX $ scaled) (thetaInit scaled 0.5)
    print pp
