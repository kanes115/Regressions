module DataBuilder
  (
  getNumber
  )where

    import System.Process
    import System.Exit

    import Data.Char
    import Data.List.Split
    import System.IO

    import Matrix hiding (decreaseAll)

    type Filepath = String
    type Header = String
    type Separator = String

    newtype DataContainer = DataContainer ([Header], Matrix (Maybe Float))

    newtype NumContainer = NumContainer ([Header], Matrix (Float))


    instance Show DataContainer where
      show (DataContainer (xs, m)) = show xs ++ "\n" ++ show m


    instance Show NumContainer where
      show (NumContainer (xs, m)) = show xs ++ "\n" ++ show m


    getDataContainer :: [String] -> Separator -> DataContainer
    getDataContainer contentInLines sep = DataContainer (splitOn sep . head $ contentInLines, transposeM $ Matrix inside)
                                where
                                  inside = map (\s -> map getNumber. splitOn sep $ s) . tail $ contentInLines

    filterDataContainer :: [Int] -> DataContainer -> NumContainer
    filterDataContainer columnsToDelete (DataContainer (s, m)) = NumContainer ( deleteElements columnsToDelete s,
                                                  fmap (\(Just e) -> e) .
                                                  filterLinesHor (\s -> if s == Nothing then False else True) .
                                                  deleteColumns columnsToDelete $ m)


    -- getData :: Filepath -> IO (Matrix Float)
    -- getData path = do
    --   handle <- openFile path ReadMode
    --   contents <- hGetContents handle
    --   return $  . snd . getDataContainer .  lines $ contents



    --PRIVATE FUNCTIONS

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
