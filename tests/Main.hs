module Main where

import Test.Tasty (defaultMain, testGroup)

import UnionFindTest

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [
    UnionFindTest.tests
  ]