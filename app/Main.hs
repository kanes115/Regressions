module Main where

import AppRunner
import Regressions
import Matrix
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    file <- parseCSV "examples/kwadraty.csv" ([], 2)
    preparedFile <- prepareFile file
    trainInfo <- flip train (0.00001, [], 1000, 10000000) preparedFile
    print trainInfo
    print $ (makeTrainingSet [5])
    print $ guessY (makeTrainingSet [5]) trainInfo
