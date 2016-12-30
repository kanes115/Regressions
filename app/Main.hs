module Main where

import Lib
import Matrix
import DataBuilder

main :: IO ()
main = do
    datacon <- (getData "examples/occupancy.csv" ",")
    numcon <- (return $ (filterDataContainer [1,2] datacon))
    print numcon
