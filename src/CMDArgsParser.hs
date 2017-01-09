{-
Arguments must be in a form:
  ./gradientDescent 
-}
module CMDArgsParser
  (

  ) where

    type CMDArgs = () -- Filepath, ColumnsToDelete, yColumnIndex, Alpha, NormalizeConst, Epsilon, MaxIter


    parseCMDargs :: [String] -> CMDArgs
    parseCMDargs
