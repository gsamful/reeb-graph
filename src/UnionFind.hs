module UnionFind where

import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe


-- TODO: Use vectors here.
data UnionFind a = UnionFind 
  { parents :: Map.Map a a
  , ranks   :: Map.Map a Int
  }

getRank :: (Ord a) => a -> UnionFind a -> Int
getRank x uf = Maybe.fromMaybe 0 $ Map.lookup x (ranks uf)

-- getParent :: (Ord a) => a -> UnionFind a -> a
-- getParent x uf = Maybe.fromMaybe x $ Map.lookup x (parents uf)

getParent :: (Ord a) => a -> State (UnionFind a) a
getParent x =
  do
    uf <- get
    return $ Maybe.fromMaybe x $ Map.lookup x (parents uf)

setParent :: (Ord a) => a -> a -> State (UnionFind a) a
setParent x p =
  do
    uf <- get
    put $ uf { parents = Map.insert x p (parents uf)}
    return p

increaseRank :: (Ord a) => a -> State (UnionFind a) Int
increaseRank x =
  do
    uf <- get
    let oldRank = getRank x uf
    let newRank = oldRank + 1
    put $ uf { ranks = Map.insert x newRank (ranks uf)}
    return newRank

empty :: (Ord a) => UnionFind a
empty = UnionFind
  { parents = Map.empty
  , ranks = Map.empty
  }

mkSet :: (Ord a) => a -> UnionFind a -> UnionFind a
mkSet x uf = 
  let
    updatedParents = Map.insert x x (parents uf)
    updatedRanks = Map.insert x 0 (ranks uf)
  in
    uf 
    { parents = updatedParents
    , ranks = updatedRanks
  }

-- find :: (Ord a) => a -> UnionFind a -> (a, UnionFind a)
-- find x uf = case Map.lookup x (parents uf) of
--   Just p
--     | p == x    -> (x, uf)
--     | otherwise -> 
--       let
--         (p2, updatedUF) = find p uf
--         updatedParents2 = Map.insert x p2 (parents updatedUF)
--       in
--         (p2, updatedUF { parents = updatedParents2 })
--   Nothing -> 
--     (x, mkSet x uf)

find :: (Ord a) => a -> State (UnionFind a) a
find x =
  do
    uf <- get
    case Map.lookup x (parents uf) of
      Just p
        | p == x    -> return x
        | otherwise ->
          do
            p2 <- find p
            setParent x p2
      Nothing ->
        do
          put $ mkSet x uf
          return x

-- union :: (Ord a) => a -> a -> UnionFind a -> UnionFind a
-- union x y uf =
--   let
--     (xRoot, uf1) = find x uf
--     (yRoot, uf2) = find y uf1
--     rankX = getRank xRoot uf2
--     rankY = getRank yRoot uf2
--   in
--     case compare rankX rankY of
--       LT ->
--         let 
--           updatedParents = Map.insert (getParent xRoot uf2) yRoot (parents uf2)
--         in
--           uf { parents = updatedParents }
--       GT ->
--         let
--           updatedParents = Map.insert (getParent yRoot uf2) xRoot (parents uf2)
--         in
--           uf { parents = updatedParents }
--       EQ
--         | xRoot /= yRoot -> 
--           let
--             updatedParents = Map.insert (getParent yRoot uf2) xRoot (parents uf2)          
--             updatedRanks = Map.insert xRoot ((getRank xRoot uf2) + 1) (ranks uf2)
--           in 
--             uf { parents = updatedParents, ranks = updatedRanks }
--         | otherwise -> uf
      
union :: (Ord a) => a -> a -> State (UnionFind a) a
union x y =
  do
    xRoot <- find x
    yRoot <- find y
    uf <- get
    let rankX = getRank xRoot uf
    let rankY = getRank yRoot uf
    ret <- case rankX `compare` rankY of
      LT ->
        do
          xParent <- getParent xRoot
          setParent xParent yRoot
          return yRoot
      GT ->
        do
          yParent <- getParent yRoot
          setParent yParent xRoot
          return xRoot
      EQ
        | xRoot /= yRoot ->
          do
            yParent <- getParent yRoot
            setParent yParent xRoot
            increaseRank xRoot
            return xRoot
        | otherwise ->
          return xRoot
    return ret