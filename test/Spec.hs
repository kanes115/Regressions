import Test.HUnit

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






-- Private functions
primesTo m = sieve [2..m]       {- (\\) is set-difference for unordered lists -}
             where
             sieve (x:xs) = x : sieve (xs Data.List.\\ [x,x+x..m])
             sieve [] = []


primeTest n = TestCase (assertEqual "Prime-size matrix." (toASquareMatrix [1..n]) (vector [1..n] 1))
