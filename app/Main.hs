module Main where

import Lib
import Matrix
import DataBuilder
import Regressions

main :: IO ()
main = do
  datacon <- (getData "examples/exampledata.csv" ",")
  numcon <- (return $ (filterDataContainer [] datacon))
  xycon <- return $ getXYContainer 2 numcon
  newfeat <- return $ createNewFeatures xycon
  scaled <- return $ scale newfeat
  freeterm <- return $ addFreeTerm scaled
  print $ freeterm
  (gradientDescent freeterm 0.001 1 0.1) >>= print




    -- datacon <- (getData "examples/occupancy.csv" ",")
    -- numcon <- (return $ (filterDataContainer [1,2] datacon))
    -- xycon <- return $ getXYContainer 6 numcon
    -- newfeat <- return $ createNewFeatures xycon
    -- scaled <- return $ scale newfeat
    -- freeterm <- return $ addFreeTerm scaled
    -- (gradientDescent freeterm 0.1 1 0.1) >>= print
