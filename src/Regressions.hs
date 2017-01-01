{-

We assume last column is the Y column. The result we're trying to predict.

TODO:
  - function factory for creating new features in different ways
-}

module Regressions
    ( createNewFeatures
    , scale
    , getXYContainer
    ) where

      import Matrix
      import DataBuilder (NumContainer(..), Header)

      type X = Matrix Float
      type Y = Matrix Float
      type Theta = Matrix Float
      data XYContainer = XYContainer ([Header], X, Y)

      instance Show XYContainer where
        show (XYContainer (headers, x, y)) = "---------X--------- \n" ++ show headers ++ "\n" ++ show x ++ "\n" ++ "---------Y--------- \n" ++ show y

      getXYContainer :: Int -> NumContainer -> XYContainer
      getXYContainer yColumnIndex (NumContainer (s, m)) = XYContainer (allBut_ yColumnIndex s, deleteColumns [yColumnIndex] m, Matrix [getColumn yColumnIndex m])





      createNewFeatures :: NumContainer -> NumContainer
      createNewFeatures (NumContainer (s , m)) = NumContainer (s ++ (map (++" squared") s) ++ (map (++ " to power 3") s),
                                                  unJust ((unJust withQuad) `conver` (fmap (^3) m))
                                                  )
                                                  where
                                                    withQuad = m `conver` (fmap (^2) m)
                                                    unJust = \(Just a) -> a

      scale :: NumContainer -> NumContainer
      scale (NumContainer (s, m)) = NumContainer (s, scaleLines m)





--PRIVATE FUNCTIONS


      -- thetaInit :: NumContainer -> Double -> Matrix a
      -- thetaInit (NumContainer (xs, _)) initval = vector (replicate (length xs) initval) 0

      allBut_ :: Int -> [a] -> [a]
      allBut_ _ [] = []
      allBut_ 1 (x:xs) = allBut_ 0 xs
      allBut_ n (x:xs) = x:(allBut_ (n-1) xs)
