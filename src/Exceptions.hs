module Exceptions
  (
      throwE
    , parseUserErrorMessge
  ) where

    import Data.List


    parseUserErrorMessge :: String -> String
    parseUserErrorMessge = reverse . tail . reverse . drop 12

    throwE :: String -> IO a
    throwE s = ioError (userError  s)
