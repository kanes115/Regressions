{-
TODO:
  - function factory for creating new features in different ways
-}

module Regressions
    ( createNewFeatures
    , scale
    ) where

      import Matrix
      import DataBuilder (NumContainer(..))


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

    -- unJust :: Maybe a -> a
    -- unJust Nothing = Nothing
    -- unJust (Just a) = a
