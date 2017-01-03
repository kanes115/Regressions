module Main where

-- import Lib
-- import Matrix
-- import DataBuilder
-- import Regressions

import AppRunner

main :: IO ()
main = do
    -- gradDescInfo <- (train "examples/exampledata.csv" [] 2 1 5.6 (0.000000000019, 0, 30, 100))
    -- print gradDescInfo

    gradDescInfo <- (train "examples/avgpricesuk.csv" [1] 3  1 3  (0.00000000000000000004, 0, 30, 10000))
    print gradDescInfo




  -- datacon <- (getData "examples/exampledata.csv" ",")
  -- numcon <- (return $ (filterDataContainer [] datacon))
  -- xycon <- return $ getXYContainer 2 numcon
  -- newfeat <- return $ createNewFeatures xycon
  -- scaled <- return $ scale newfeat
  -- freeterm <- return $ addFreeTerm scaled
  -- print $ freeterm
  -- (gradientDescent freeterm 0.000000000019 0 30) >>= print




    -- datacon <- (getData "examples/occupancy.csv" ",")
    -- numcon <- (return $ (filterDataContainer [1,2] datacon))
    -- xycon <- return $ getXYContainer 6 numcon
    -- newfeat <- return $ createNewFeatures xycon
    -- --scaled <- return $ scale newfeat
    -- freeterm <- return $ addFreeTerm newfeat
    -- (gradientDescent freeterm 0.1 0 0.4) >>= print
