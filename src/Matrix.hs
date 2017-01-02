{-
Module Matrix provides simple operatons on 2d matrixes.

Things to rememember:
  -Matrix [[]] - lista list. listy w liście to kolumny macierzy.
-}


module Matrix
  ( Matrix(..)
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
  , unpackM
  , packM
  ) where

    import Data.List

    type Axis = Int   --  0 - pion, 1 - ¬poziom
    type Width = Int
    type Height = Int

    data Matrix a = EmptyM | Matrix [[a]]

    instance Show a => Show (Matrix a) where
      show EmptyM = "Empty matrix"
      show matrix = concat . (map concat) . (map ((++["\n"]) . (map ((++" ") . show) ))) . unpackM . transposeM $ matrix

    instance Functor Matrix where
      fmap _ EmptyM = EmptyM
      fmap f (Matrix [x]) = packM $ [map f x]
      fmap f (Matrix (x:xs)) = packM $ (map f x):(unpackM (fmap f (packM xs)))

    instance (Num a) => Num (Matrix a) where
      (+) = zipWithM (+)
      fromInteger x = toASquareMatrix [fromInteger x]
      abs = fmap abs
      signum =  fmap signum
      negate = fmap negate
      (*) a b = transposeM (mulling (transposeM a) b)


    instance Eq a => Eq (Matrix a) where
      (==) a b = (all id) $ (map (all id)) $ unpackM $ (zipWithM (==)) a b


    mulling :: Num a => Matrix a -> Matrix a -> Matrix a        -- multiplies two matrixes where first is already transposed
    mulling (Matrix [x]) (Matrix ys) = (Matrix [map (scalarMulLines x) ys])
    mulling (Matrix (x:xs)) (Matrix (ys)) = unJust ((Matrix [map (scalarMulLines x) ys]) `conver` (mulling (Matrix xs) (Matrix ys)))
                              where
                                unJust = \(Just a) -> a


    emptyM :: Matrix a
    vector :: [a] -> Axis -> Matrix a
    zipWithM :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
    transposeM :: Matrix a -> Matrix a
    howManyElements :: Matrix a -> Int
    findAMostQuadraticEdgeSize :: [a] -> (Width, Height)
    findEdgeSizes :: [a] -> [(Width, Height)]
    toASquareMatrix :: [a] -> Matrix a
    toRectMatrix :: Width -> Height -> [a] -> Matrix a
    toHeightMatrix :: Height -> [a] -> Matrix a
    toList :: Matrix a -> [a]
    normalize :: Matrix a -> Matrix a
    zipWithLines :: (a -> a -> a) -> Matrix a -> Matrix a
    conver :: Matrix a -> Matrix a -> Maybe (Matrix a)
    conhor :: Matrix a -> Matrix a -> Maybe (Matrix a)
    filterLinesHor :: (a -> Bool) -> Matrix a -> Matrix a     --checks if all of elements in horizontal line fulfill predicate, leaves those lines that fulfill
    filterLinesVer :: (a -> Bool) -> Matrix a -> Matrix a     --checks if all of elements in vertical line fulfill predicate, leaves those lines that fulfill
    deleteColumns :: [Int] -> Matrix a -> Matrix a            --deletes all the columns specified in the list given as argument



    emptyM = EmptyM

    vector [] _ = EmptyM
    vector xs 0 = Matrix [xs]
    vector xs 1 = Matrix (makeListOfOneElementLists xs)


    transposeM EmptyM = EmptyM
    transposeM (Matrix [[]]) = EmptyM
    transposeM (Matrix []) = EmptyM
    transposeM (Matrix [x]) = Matrix $ makeListOfOneElementLists x
    transposeM (Matrix (x:xs)) = concatHorizM (Matrix (makeListOfOneElementLists x)) (transposeM (Matrix xs))

    zipWithM _ EmptyM s = error "Can't zip with empty matrix!"
    zipWithM _ s EmptyM = error "Can't zip with empty matrix!"
    zipWithM f (Matrix [x]) (Matrix [y]) = (packM $ [zipWith f x y])
    zipWithM f (Matrix (x:xs)) (Matrix (y:ys)) = (packM $ [zipWith f x y]) `concatVerticM` (zipWithM f (Matrix xs) (Matrix ys))

    findEdgeSizes xs = [(a, b) | a <- [(floor . sqrt . fromIntegral $ len)..len], b <- [1..len], a * b == len]
        where len = length xs


    toRectMatrix _  _ []= EmptyM
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
    deleteColumns _ EmptyM = EmptyM
    deleteColumns _ (Matrix []) = EmptyM
    deleteColumns _ (Matrix [[]]) = EmptyM
    deleteColumns (x:xs) m = deleteColumns (decreaseAll xs) (deleteColumn x m)

    scaleLines :: (Num a, Fractional a, Real a) => Matrix a -> Matrix a
    scaleLines (Matrix [x]) = (Matrix [map (\e -> (e - (avg x)/(range x))) x])
    scaleLines (Matrix (x:xs)) = unJust ((Matrix ([map (\e -> ((e - (avg x))/(range x))) x])) `conver` (scaleLines (Matrix xs)))
          where
            unJust = \(Just a) -> a


    getColumn :: Int -> Matrix a -> Matrix a
    getColumn n (Matrix xs) = packM $ [loop n xs]
      where
        loop _ [] = []
        loop 1 (y:ys) = y
        loop n (y:ys) = (loop (n-1) ys)

    --PRIVATE FUNCTIONS

    scalarMulLines :: Num a => [a] -> [a] -> a
    scalarMulLines [] [] = error "empty lists"
    scalarMulLines xs ys = case length xs == length ys of
            False -> error "must have same sizes"
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
    addLine xs 0 EmptyM = Matrix [xs]
    addLine xs 1 EmptyM = Matrix $ makeListOfOneElementLists xs
    addLine xs 0 (Matrix ys) = Matrix (xs:ys)
    addLine (x:xs) 1 (Matrix (y:ys)) = concatVerticM (Matrix [x:y]) (addLine xs 1 (Matrix ys))

    makeListOfOneElementLists :: [a] -> [[a]]
    makeListOfOneElementLists [x] = [[x]]
    makeListOfOneElementLists (x:xs) = [[x]] ++ (makeListOfOneElementLists xs)

    unpackM :: Matrix a -> [[a]]
    unpackM (Matrix xs) = xs

    packM :: [[a]] -> Matrix a
    packM = Matrix

    concatVerticM :: Matrix a -> Matrix a -> Matrix a
    (Matrix xs) `concatVerticM` (Matrix ys) = Matrix (xs ++ ys)

    concatHorizM ::  Matrix a -> Matrix a -> Matrix a
    concatHorizM EmptyM m = m
    concatHorizM m EmptyM = m
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
