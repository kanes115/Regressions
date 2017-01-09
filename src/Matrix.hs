{-|
Module Matrix provides simple operatons on 2d matrixes and matrix datatype as well.

  Matrix is represented as list of lists. Each list in the list is a column.
-}


module Matrix
  ( Matrix
  , emptyM
  , vector
  , zipWithM
  , transposeM
  , howManyElements
  , toRectMatrix
  , findEdgeSizes
  , findAMostQuadraticEdgeSize
  , toASquareMatrix
  , toHeightMatrix
  , toList
  , normalize
  , zipWithLines
  , conver
  , conhor
  , filterLinesHor
  , filterLinesVer
  , deleteColumns
  , scaleLines
  , getColumn
  , getRow
  , unpackM
  , packM
  , getElementByInd
  , getSize
  , getWidth
  , getHeight
  , getAvgColumnsMatrix
  , getRangeColumnsMatrix
  ) where

    import Data.List

    -- | Specifies the axis: 0 - vertical, 1  - horizontal
    type Axis = Int
    -- | Width or the horizontal index (column index in matrix)
    type Width = Int
    -- | Height or the vertical index (row index in matrix)
    type Height = Int

    -- | This is Matrix, represented as list of lists in a package
    data Matrix a = Matrix [[a]]

    -- * Instances

    instance Show a => Show (Matrix a) where
      show (Matrix []) = "Empty matrix."
      show matrix = concat . (map concat) . (map ((++["\n"]) . (map ((++" ") . show) ))) . unpackM . transposeM $ matrix

    instance Functor Matrix where
      fmap _ (Matrix []) = (Matrix [])
      fmap f (Matrix [x]) = packM $ [map f x]
      fmap f (Matrix (x:xs)) = packM $ (map f x):(unpackM (fmap f (packM xs)))

    instance (Num a) => Num (Matrix a) where
      (+) = zipWithM (+)
      fromInteger x = toASquareMatrix [fromInteger x]
      abs = fmap abs
      signum =  fmap signum
      negate = fmap negate
      (*) a b = case getHeight a == getWidth b of
        True -> transposeM (mulling (transposeM a) b)
        False -> error ("(*) : Height of the first matrix must be equal to width of the second but has: \n height 1st = " ++ (show . getHeight $ a) ++ ", width 2nd = " ++ (show . getWidth $ b))


    instance Eq a => Eq (Matrix a) where
      (==) a b = (all id) $ (map (all id)) $ unpackM $ (zipWithM (==)) a b

    -- * FUNCTIONS INDEX
    -- | Gives an empty matrix
    emptyM :: Matrix a
    -- | Packs a list of lists into a matrix
    packM :: [[a]] -> Matrix a
    -- | Gives a list of lists out of matrix
    unpackM :: Matrix a -> [[a]]
    -- | Gives a vector
    vector :: [a] -> Axis -> Matrix a
    -- | Zips corresponding elements using function
    zipWithM :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
    -- | Transposes matrix
    transposeM :: Matrix a -> Matrix a
    -- | Gives a number of elements in matrix
    howManyElements :: Matrix a -> Int
    -- | Gives a pair of sizes in a tuple (Width, Height) most close to square in which matrix can be represented
    findAMostQuadraticEdgeSize :: [a] -> (Width, Height)
    -- | Gives pairs of sizes as tuples (Width, Height) in which matrix can be represented
    findEdgeSizes :: [a] -> [(Width, Height)]
    -- | Gives a matrix of sizes most close to square
    toASquareMatrix :: [a] -> Matrix a
    -- | Gives a matrix of sizes specified in arguments.
    toRectMatrix :: Width -> Height -> [a] -> Matrix a
    -- | Gives a matrix of height Height and width counted so that it stays rectangular
    toHeightMatrix :: Height -> [a] -> Matrix a
    -- | Return flat list of elements of matrix
    toList :: Matrix a -> [a]
    -- | Makes matrix as quadratic as possible
    normalize :: Matrix a -> Matrix a
    -- | Zips columns using specified function.
    zipWithLines :: (a -> a -> a) -> Matrix a -> Matrix a
    -- | Concatenates matrixes in vertical axis. Possible infix notation
    conver :: Matrix a -> Matrix a -> Maybe (Matrix a)
    -- | Concatenates matrixes in horizontal axis. Possible infix notation
    conhor :: Matrix a -> Matrix a -> Maybe (Matrix a)
    -- | Checks if all of elements in horizontal line fulfill predicate, leaves those lines that fulfill
    filterLinesHor :: (a -> Bool) -> Matrix a -> Matrix a
    -- | Checks if all of elements in vertical line fulfill predicate, leaves those lines that fulfill
    filterLinesVer :: (a -> Bool) -> Matrix a -> Matrix a
    -- | Deletes all the columns specified as their indexes in the list given as argument, indexing from 1
    deleteColumns :: [Int] -> Matrix a -> Matrix a
    -- | Scales columns, it substracts the average of column and devides by range of elements in a column for each element of a column.
    scaleLines :: (Num a, Fractional a, Real a) => Matrix a -> Matrix a
    -- | Gives a horizontal vector of tuples (avg of column, range of column)
    getAvgRangeAboutColumns :: (Num a, Fractional a, Real a) => Matrix a -> Matrix (a, a)

    -- ** getters
    -- | Returns size of matrix as tuple (Width, Height)
    getSize :: Matrix a -> (Width, Height)
    -- | Returns Width of matrix
    getWidth :: Matrix a -> Width
    -- | Returns Height of matrix
    getHeight :: Matrix a -> Height
    -- | Returns element that is on index (Width, Height), indexing from 1
    getElementByInd :: (Width, Height) -> Matrix a -> a
    -- | Returns n-th column as a vertical vector, indexing from 1
    getColumn :: Int -> Matrix a -> Matrix a
    -- | Returns n-th row as a horizontal vector, indexing from 1
    getRow :: Int -> Matrix a -> Matrix a
    -- | Retruns a horizontal vector of averages for each column
    getAvgColumnsMatrix :: (Num a, Fractional a, Real a) => Matrix a -> Matrix a
    -- | Retruns a horizontal vector of ranges for each column
    getRangeColumnsMatrix :: (Num a, Fractional a, Real a) => Matrix a -> Matrix a


    --BODIES

    emptyM = (Matrix [])

    vector [] _ = (Matrix [])
    vector xs 0 = Matrix [xs]
    vector xs 1 = Matrix (makeListOfOneElementLists xs)


    transposeM (Matrix []) = (Matrix [])
    transposeM (Matrix [x]) = Matrix $ makeListOfOneElementLists x
    transposeM (Matrix (x:xs)) = concatHorizM (Matrix (makeListOfOneElementLists x)) (transposeM (Matrix xs))

    zipWithM _ (Matrix []) (Matrix []) = Matrix []
    zipWithM _ (Matrix []) s = error ("Can't zip matrixes with different sizes!")
    zipWithM _ s (Matrix []) = error ("Can't zip matrixes with different sizes!")
    zipWithM f (Matrix [x]) (Matrix [y]) = (packM $ [zipWith f x y])
    zipWithM f (Matrix (x:xs)) (Matrix (y:ys)) = (packM $ [zipWith f x y]) `concatVerticM` (zipWithM f (Matrix xs) (Matrix ys))

    findEdgeSizes xs = [(a, b) | a <- [(floor . sqrt . fromIntegral $ len)..len], b <- [1..len], a * b == len]
        where len = length xs


    toRectMatrix _  _ [] = (Matrix [])
    toRectMatrix _  _ [x] = Matrix [[x]]
    toRectMatrix 1 height xs = addLine xs 0 emptyM
    toRectMatrix width height xs = addLine (fst . splitAt height $ xs) 0 (toRectMatrix (width - 1) height (snd . splitAt height $ xs))

    toASquareMatrix xs = toRectMatrix (fst . findAMostQuadraticEdgeSize $ xs) (snd . findAMostQuadraticEdgeSize $ xs) xs

    howManyElements (Matrix (x:xs)) = length x * (length xs + 1)

    findAMostQuadraticEdgeSize xs = head (findEdgeSizes xs)

    toHeightMatrix height xs = toRectMatrix (quot (length xs) height) height xs

    toList (Matrix [x]) = x
    toList (Matrix (x:xs)) = x ++ toList (Matrix xs)

    normalize m = toASquareMatrix . toList $ m

    zipWithLines f (Matrix (x:xs)) = vector (foldr (zipWith f) x xs) 0

    (Matrix (x:xs)) `conver` (Matrix (y:ys)) = case length x == length y of
        True -> Just $ concatVerticM (Matrix (x:xs)) (Matrix (y:ys))
        False -> Nothing

    (Matrix (xs)) `conhor` (Matrix (ys)) = case length xs == length ys of
      True -> Just $ concatHorizM (Matrix xs) (Matrix ys)
      False -> Nothing


    filterLinesHor f = transposeM . packM . filter (all f) . unpackM . transposeM

    filterLinesVer f = packM . filter (all f) . unpackM

    deleteColumns [] m = m
    deleteColumns _ (Matrix []) = (Matrix [])
    deleteColumns (x:xs) m = deleteColumns (decreaseAll xs) (deleteColumn x m)

    scaleLines (Matrix [x]) = (Matrix [map (\e -> (e - (avg x)/(range x))) x])
    scaleLines (Matrix (x:xs)) = unJust ((Matrix ([map (\e -> ((e - (avg x))/(range x))) x])) `conver` (scaleLines (Matrix xs)))


    getAvgRangeAboutColumns (Matrix []) = (Matrix [])
    getAvgRangeAboutColumns (Matrix [x]) = (Matrix [[(avg x, range x)]])
    getAvgRangeAboutColumns (Matrix (x:xs)) = unJust ((Matrix [[(avg x, range x)]]) `conver` (getAvgRangeAboutColumns (Matrix xs)))


    getAvgColumnsMatrix = fmap fst . getAvgRangeAboutColumns

    getRangeColumnsMatrix = fmap snd . getAvgRangeAboutColumns

    --getters

    getSize (Matrix (x:xs)) = (length (x:xs), length x)


    getWidth = fst . getSize


    getHeight = snd . getSize


    getElementByInd (w, h) (Matrix xs) = case w > (length xs) || h > (length . head $ xs) of
      True -> error $ "getElementById: Index out of bounds. For w: " ++ show w ++ " and h: " ++ show h ++ " in matrix of sizes: " ++ show (length xs) ++ " x " ++ show (length . head $ xs)
      False -> loop h (loop w xs)
      where
        loop 1 [] = error "Index out of bounds. \n for 1"
        loop 1 (x:xs) = x
        loop n (x:xs) = loop (n-1) xs
        loop n [] = error ("Index out of bounds. for "++ show n)

    getRow n = transposeM . getColumn n . transposeM


    getColumn n (Matrix xs) = packM $ [loop n xs]
          where
            loop _ [] = []
            loop 1 (y:ys) = y
            loop n (y:ys) = (loop (n-1) ys)

    --PRIVATE FUNCTIONS

    mulling :: Num a => Matrix a -> Matrix a -> Matrix a        -- multiplies two matrixes where first is already transposed
    mulling (Matrix [x]) (Matrix ys) = (Matrix [map (scalarMulLines x) ys])
    mulling (Matrix (x:xs)) (Matrix (ys)) = unJust ((Matrix [map (scalarMulLines x) ys]) `conver` (mulling (Matrix xs) (Matrix ys)))


    scalarMulLines :: Num a => [a] -> [a] -> a
    scalarMulLines [] [] = error "empty lists"
    scalarMulLines xs ys = case length xs == length ys of
            False -> error ("ScalarMulLines: Lists must have same sizes but have: " ++ (show .length $ xs) ++ " and " ++ (show . length $ ys))
            True -> loop xs ys
              where
                loop [] [] = 0
                loop (a:as) (b:bs) = a*b + loop as bs

    avg :: (Num a, Real a, Fractional a) => [a] -> a
    avg xs = realToFrac (sum xs) / genericLength xs

    range :: (Num a, Ord a) => [a] -> a
    range xs = maximum xs - minimum xs


    decreaseAll :: [Int] -> [Int]
    decreaseAll [] = []
    decreaseAll (x:xs) = (x-1):(decreaseAll xs)

    deleteColumn :: Int -> Matrix a -> Matrix a
    deleteColumn a (Matrix (x:xs)) = packM $ loop a (x:xs)
          where
            loop _ [] = []
            loop 1 (y:ys) = loop 0 ys
            loop n (y:ys) = y:(loop (n-1) ys)


    addLine :: [a] -> Axis -> Matrix a -> Matrix a
    addLine [] _ s = s
    addLine xs 0 (Matrix []) = Matrix [xs]
    addLine xs 1 (Matrix []) = Matrix $ makeListOfOneElementLists xs
    addLine xs 0 (Matrix ys) = Matrix (xs:ys)
    addLine (x:xs) 1 (Matrix (y:ys)) = concatVerticM (Matrix [x:y]) (addLine xs 1 (Matrix ys))

    makeListOfOneElementLists :: [a] -> [[a]]
    makeListOfOneElementLists [x] = [[x]]
    makeListOfOneElementLists (x:xs) = [[x]] ++ (makeListOfOneElementLists xs)

    unpackM (Matrix xs) = xs
    packM = Matrix

    concatVerticM :: Matrix a -> Matrix a -> Matrix a
    (Matrix xs) `concatVerticM` (Matrix ys) = Matrix (xs ++ ys)

    concatHorizM ::  Matrix a -> Matrix a -> Matrix a
    concatHorizM (Matrix []) m = m
    concatHorizM m (Matrix []) = m
    concatHorizM (Matrix [x]) (Matrix [y]) = Matrix ([x ++ y])
    (Matrix (x:xs)) `concatHorizM` (Matrix (y:ys)) = concatVerticM (Matrix [x ++ y]) (concatHorizM (Matrix xs) (Matrix ys))

    nLenListOfOnes :: Int -> [Int]
    nLenListOfOnes 0 = []
    nLenListOfOnes 1 = [1]
    nLenListOfOnes n = [1] ++ nLenListOfOnes (n-1)


    lowerFstTriple :: Ord a => (a,a,a) -> (a,a,a) -> (a,a,a)
    lowerFstTriple (x,y,z) (a,b,c) = case x < a of
                  True -> (x,y,z)
                  False -> (a,b,c)

    unJust :: Maybe a -> a
    unJust (Just a) = a
    unJust Nothing = error " "
