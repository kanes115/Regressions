module Main where

import AppRunner
import Regressions
import Matrix
import System.Environment
import Control.Exception

main :: IO ()
main = (do
    args <- getArgs

    -- file <- parseCSV "examples/occupancy.csv" ([1, 2], 7)
    -- preparedFile <- prepareFile file
    -- trainInfo <- flip train (0.00001, [], 0.01, 10000000) preparedFile
    -- print trainInfo
    -- return ()


    file <- parseCSV "examples/exampledata.csv" ([], 2)   --(ColumnsToDelete, YColumnIndex)
    preparedFile <- prepareFile file
    trainInfo <- flip train (0.000001, [], 0.000001, 10000000) preparedFile   --(alpha, lista (normconst,var), epsilon, maxiter)
    print trainInfo
    return ()

    ) `catch` handler


handler :: IOError -> IO ()
handler = print
