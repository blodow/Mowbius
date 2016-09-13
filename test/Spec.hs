import Mowbius

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMainWithOpts
  [ testCase "rdp" testRDP
  ] mempty

testRDP :: Assertion
testRDP = simplify [p] @?= [expected]
 where
  p = [(0,0), (1,0), (2,0), (3,1), (4,0)]
  expected = [(0,0), (2,0), (3,1), (4,0)]


