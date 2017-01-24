{-
  This is a module for reading and parsing *.csv files.
-}

module DataBuilder
  ( getData
  , filterDataContainer
  , getMatrixFromDataContainer
  , getMatrixFromNumContainer
  , NumContainer(..)
  , Header
  , DataContainer
  )where


    import Data.Char
    import Data.List.Split
    import System.IO
    import Control.Exception
    import Exceptions

    import Matrix hiding (decreaseAll)

    -- * Types
    -- | Simply String containing path to a file§
    type Filepath = String
    type Header = String
    -- | String that seperates data, in csv it is: ","
    type Separator = String

    -- | DataContainer is a container for headers of data and data itself
    newtype DataContainer = DataContainer ([Header], Matrix String)
    -- | NumContainer stores the same as DataContainer except non-numerical values
    newtype NumContainer = NumContainer ([Header], Matrix Double)

    -- * Instances
    instance Show DataContainer where
      show (DataContainer (xs, m)) = show xs ++ "\n" ++ show m


    instance Show NumContainer where
      show (NumContainer (xs, m)) = show xs ++ "\n" ++ show m


    -- * FUNCTION INDEX
    -- | Takes list of indexes of columns that are to be deleted, DataContainer with data and return NumContainer with only numeric values.
    -- | If it cannot get a number out of a field it removes this one training set.
    filterDataContainer :: [Int] -> DataContainer -> NumContainer
    -- | Takes a path to the file, seperator that seperates data and returns DataContainer
    getData :: Filepath -> Separator -> IO (DataContainer)


    -- BODIES
    filterDataContainer columnsToDelete (DataContainer (s, m)) = NumContainer ( deleteElements columnsToDelete s,
                                                  fmap (\(Just e) -> e) .
                                                  filterLinesHor (\s -> if s == Nothing then False else True) .
                                                  fmap getNumber .
                                                  deleteColumns columnsToDelete $ m)



    getData path sep = (do
      handle <- openFile path ReadMode
      contents <- hGetContents handle
      if isFileOK contents
        then return  (getDataContainer sep . lines $ contents)
        else throwE "\n The file is not a valid csv file.")
      `catch` handler


    handler :: IOError -> IO (DataContainer)
    handler e = do
      throwE (show e ++ "Error while getting data.")




    getMatrixFromDataContainer :: DataContainer -> Matrix String
    getMatrixFromDataContainer (DataContainer (_, m)) = m

    getMatrixFromNumContainer :: NumContainer -> Matrix Double
    getMatrixFromNumContainer (NumContainer (_, m)) = m

    --PRIVATE FUNCTIONS

    isFileOK :: String -> Bool    -- here we can evaluate on errors that can appear
    isFileOK s
      | length s == 0 = False     -- when empty file
      | otherwise = True

    getDataContainer :: Separator -> [String] -> DataContainer
    getDataContainer sep contentInLines = DataContainer (map filterHeader . splitOn sep . head $ contentInLines, transposeM $ (packM inside))
                                where
                                  inside = map (\s -> splitOn sep $ s) . tail $ contentInLines

    filterHeader :: Header -> Header
    filterHeader s = filter (\ch -> (ch /= '\"') && (ch /= '\'')) s

    deleteElements :: [Int] -> [a] -> [a]
    deleteElements [] l = l
    deleteElements _ [] = []
    deleteElements (a:as) xs = deleteElements (decreaseAll (as)) (deleteElement a xs)


    deleteElement :: Int -> [a] -> [a]
    deleteElement a xs  = loop a xs
          where
            loop _ [] = []
            loop 1 (y:ys) = loop 0 ys
            loop n (y:ys) = y:(loop (n-1) ys)


    decreaseAll :: [Int] -> [Int]
    decreaseAll [] = []
    decreaseAll (x:xs) = (x-1):(decreaseAll xs)


    getNumber :: String -> Maybe Double
    getNumber s = case head s == '\"' || head s == '\'' || (length s == 0) ||(isLetter . head $ s) of
      True -> Nothing
      False -> Just $ (read s :: Double)
