module UnionFind where

import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data UnionFind a = UnionFind 
  { parents :: Map.Map a a
  , ranks   :: Map.Map a Int
  }

empty :: (Ord a) => UnionFind a
empty = UnionFind
  { parents = Map.empty
  , ranks = Map.empty
  }

makeSet :: (Ord a) => a -> State (UnionFind a) ()
makeSet x = modify (\uf -> 
  uf
    { parents = Map.insert x x (parents uf)
    , ranks   = Map.insert x 0 (ranks uf)
    }    
  )

-- Gets the rank of the supplied element.  Performs a "makeSet"
-- operation in the event that the element is not in the data
-- structure.
getRank :: (Ord a) => a -> State (UnionFind a) Int
getRank x =
  do
    uf <- get
    case Map.lookup x (ranks uf) of
      Just rank ->
        return rank
      Nothing ->
        do
          makeSet x
          return 0

-- Gets the parent of the supplied element.  Performs a "makeSet"
-- operation in the event that the element is not in the data
-- structure.
getParent :: (Ord a) => a -> State (UnionFind a) a
getParent x =
  do
    uf <- get
    case Map.lookup x (parents uf) of
      Just parent ->
        return parent
      Nothing ->
        do
          makeSet x
          return x

setParent :: (Ord a) => a -> a -> State (UnionFind a) a
setParent x p =
  do
    modify (\uf -> uf { parents = Map.insert x p (parents uf) })
    return p

increaseRank :: (Ord a) => a -> State (UnionFind a) Int
increaseRank x =
  do
    oldRank <- getRank x
    let newRank = oldRank + 1
    modify (\uf -> uf { ranks = Map.insert x newRank (ranks uf)})
    return newRank 

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
          makeSet x
          return x

union :: (Ord a) => a -> a -> State (UnionFind a) a
union x y =
  do
    xRoot <- find x
    yRoot <- find y
    rankX <- getRank xRoot
    rankY <- getRank yRoot
    
    case rankX `compare` rankY of
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