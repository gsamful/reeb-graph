module Simplex where

-- A couple of auxiliary types to prevent conflating between various Int-valued types.
data InputPointId = InputPointId { iToInt :: Int } deriving (Show, Eq, Ord)
data FunctionValueRank = FunctionValueRank { rToInt :: Int } deriving (Show, Eq, Ord)
data CloneId = CloneId { cToInt :: Int } deriving (Show, Eq, Ord)

data InputVertex = InputVertex
  { vertexId :: InputPointId
  , fRank    :: FunctionValueRank
  } deriving (Show, Eq)

data CloneVertex = CloneVertex
  { inputVertex :: InputVertex
  , cloneId     :: CloneId
  } deriving (Show, Eq)

type Vertex = Either CloneVertex InputVertex

data Triangle = Triangle
  { v1 :: Vertex
  , v2 :: InputVertex
  , v3 :: Vertex
  } deriving (Show, Eq)

instance Ord InputVertex where
  v1 `compare` v2 = (fRank v1, vertexId v1) `compare` (fRank v2, vertexId v2)

instance Ord CloneVertex where
  c1 `compare` c2 = (inputVertex c1, cloneId c1) `compare` (inputVertex c2, cloneId c2)

instance Ord Triangle where
  t1 `compare` t2 = (v2 t1) `compare` (v2 t2)