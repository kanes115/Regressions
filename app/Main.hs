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
    freeterm <- return $ addFreeTerm scaled
    print (gradientDescent freeterm 0.8 10 1)
