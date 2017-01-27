import Test.HUnit
-- import Test.QuickCheck

import Matrix
import Data.List

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestList (primeTests ++ [transposeTest] ++ [testGetSize])


primeTests = (map (primeTest) (filter (>50) . primesTo $ 3000))   -- For lists of size that is a prime matrix should always be a vector


transposeTest = TestCase (assertEqual "transposeM: " (transposeM a) (b))
        where
          a = vector [1..100] 0
          b = vector [1..100] 1


testGetSize = TestCase (assertEqual "SquareMatrix: " (getSize $ toASquareMatrix [1..100]) (10, 10))



{-  Testy parametryczne
prop_przemiennoscDod :: Num a => Matrix a -> Matrix a -> Bool
prop_przemiennosc n m = n + m == m + n

prop_przechodnioscDod :: Num a => Matrix a -> Matrix a -> Bool
prop_przechodniosc a b c = (a + b) + c == a + (b + c)

prop_przechodnioscMno :: Num a => Matrix a -> Matrix a -> Bool
prop_przechodnioscMno a b c = (a * b) * c == a * (b * c)

prop_przemiennoscMno :: Num a => Matrix a -> Matrix a -> Bool
prop_przemiennoscMno a b = a * b == b * a
-}

-- Private functions
primesTo m = sieve [2..m]       {- (\\) is set-difference for unordered lists -}
             where
             sieve (x:xs) = x : sieve (xs Data.List.\\ [x,x+x..m])
             sieve [] = []


primeTest n = TestCase (assertEqual "Prime-size matrix." (toASquareMatrix [1..n]) (vector [1..n] 1))
