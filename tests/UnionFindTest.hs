{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UnionFindTest
  ( tests
  ) where

import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (testCase, assertEqual)
import Test.HUnit              (Assertion, (@?=))
import UnionFind
import Control.Monad.State

abcde :: State (UnionFind [Char]) ()
abcde = do
  makeSet "a"
  makeSet "b"
  makeSet "c"
  makeSet "d"
  makeSet "e"

abcde2 = abcde >> abcde

eval_abcde :: State (UnionFind [Char]) a -> a
eval_abcde op = 
  evalState (
      abcde >>
      op
    ) empty

test01 :: Assertion
test01 = (eval_abcde $ sequence $ getParent <$> ["a", "b", "c", "d", "e"]) @?= ["a", "b", "c", "d", "e"]

test02 :: Assertion
test02 = (eval_abcde $ getParent "f") @?= "f"

test03 :: Assertion
test03 = (eval_abcde $ sequence $ getRank <$> ["a", "b", "c", "d", "e"]) @?= [0, 0, 0, 0, 0]

test04 :: Assertion
test04 = (eval_abcde $ increaseRank "a") @?= 1

test05 :: Assertion
test05 =
  let op = do
             union "a" "b"
             pa <- getParent "a"
             pb <- getParent "b"
             return (pa == pb)
  in
    (eval_abcde op) @?= True

tests :: TestTree
tests = testGroup "UnionFind"
  [ testCase "test01" test01
  , testCase "test02" test02
  , testCase "test03" test03
  , testCase "test04" test04
  ]