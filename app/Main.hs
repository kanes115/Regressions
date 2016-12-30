module Main where

import Lib
import Matrix
import DataBuilder
import Regressions

main :: IO ()
main = do
    datacon <- (getData "examples/occupancy.csv" ",")
    numcon <- (return $ (filterDataContainer [1,2] datacon))
    newfeat <- return $ createNewFeatures numcon
    scaled <- return $ scale newfeat
    print scaled
