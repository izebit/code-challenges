module UnitTests where

import Main 
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tokenTests 

tokenTests :: TestTree
tokenTests = testGroup "Simple Math Tests"
  [ testCase "check that string is split into correct tokens" $
      justFunction @?= 1
  ]
