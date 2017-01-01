{-
This is a module for reading and parsing *.csv files.
-}

module DataBuilder
  ( getData
  , filterDataContainer
  , NumContainer(..)
  , Header
  )where


    import Data.Char
    import Data.List.Split
    import System.IO

    import Matrix hiding (decreaseAll)

    type Filepath = String
    type Header = String
    type Separator = String

    newtype DataContainer = DataContainer ([Header], Matrix String)

    newtype NumContainer = NumContainer ([Header], Matrix Float)


    instance Show DataContainer where
      show (DataContainer (xs, m)) = show xs ++ "\n" ++ show m


    instance Show NumContainer where
      show (NumContainer (xs, m)) = show xs ++ "\n" ++ show m


    getDataContainer :: Separator -> [String] -> DataContainer
    getDataContainer sep contentInLines = DataContainer (map filterHeader . splitOn sep . head $ contentInLines, transposeM $ Matrix inside)
                                where
                                  inside = map (\s -> splitOn sep $ s) . tail $ contentInLines

    filterDataContainer :: [Int] -> DataContainer -> NumContainer
    filterDataContainer columnsToDelete (DataContainer (s, m)) = NumContainer ( deleteElements columnsToDelete s,
                                                  fmap (\(Just e) -> e) .
                                                  filterLinesHor (\s -> if s == Nothing then False else True) .
                                                  fmap getNumber .
                                                  deleteColumns columnsToDelete $ m)


    getData :: Filepath -> Separator -> IO (DataContainer)
    getData path sep = do
      handle <- openFile path ReadMode
      contents <- hGetContents handle
      return  (getDataContainer sep . lines $ contents)



    --PRIVATE FUNCTIONS

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


    getNumber :: String -> Maybe Float
    getNumber s = case head s == '\"' || head s == '\'' of
      True -> Nothing
      False -> Just $ (read s :: Float)
