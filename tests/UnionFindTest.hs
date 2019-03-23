{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UnionFindTest
  ( tests
  ) where

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
import           Test.HUnit              (Assertion, (@?=))

test01 :: Assertion
test01 = undefined
-- test01 =
--     123 @?= 321

tests :: TestTree
tests = testGroup "UnionFind"
  [ testCase "test01" test01
  ]