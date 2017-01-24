module MaybeResult
  ( MaybeRes(..)
  , getResult
  , getError
  , isError
  , isOK
  ) where

    data MaybeRes a = JustRes a | Error String

    getResult :: MaybeRes a -> a
    getResult (JustRes x) = x
    getResult e = error
     ("Remember about using 'isError'. The error was: " ++ (getError e))

    isError :: MaybeRes a -> Bool
    isError (Error _) = True
    isError (JustRes _) = False

    isOK :: MaybeRes a -> Bool
    isOK = not . isError

    getError :: MaybeRes a -> String
    getError (Error s) = s
    getError _ = error "It is not an error"


    instance Functor MaybeRes where
      fmap _ (Error s) = Error s
      fmap f (JustRes x) = JustRes (f x)

    instance Applicative MaybeRes where
      pure = JustRes
      (Error s) <*> _ = Error s
      (JustRes f) <*> sth = fmap f sth

    instance Monad MaybeRes where
      return = JustRes
      Error s >>= _ = Error s
      d >>= f = getResult . fmap f $ d
      x >> y = x >>= \_ -> y
      fail = error
