import Test.HUnit

import Matrix

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]


test1 = TestCase (assertEqual "for (foo 3)," (4) (5))
test2 = TestCase (assertEqual "for " (toASquareMatrix [1..1006]) (packM [[1..100]]))
