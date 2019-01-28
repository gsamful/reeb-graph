module Off where

import Text.Read
import Simplex as S
import Data.List (sort, sortBy)
import qualified Data.Map.Strict as Map

{-
TODO:  This is an extremely rudimentary, and frankly extremely crappy, parser.
       It's temporary.

       Obviously we want to use the state monad here to trundle around the 'restX' values.
       But not today, it can wait.

       And the return value should be a record rather than a tuple.
-}

data FunctionCoordinate = X | Y | Z deriving (Show, Eq)

data Point3D = Point3D
  { x :: Double
  , y :: Double
  , z :: Double
  } deriving (Show, Eq)

data Face3 = Face3
  { p :: Int
  , q :: Int
  , r :: Int
  } deriving (Show, Eq)


parseOffFile :: [[Char]] -> FunctionCoordinate -> Either String [Triangle]
parseOffFile lines funcCoord = 
  do
    (line1, rest1)       <- nextLine lines
    _                    <- parseFirstLine line1
    (line2, rest2)       <- nextLine rest1
    (v, f, e)            <- parseVerticesFacesEdges line2
    (vertices, rest3)    <- case splitAt v rest2 of
                              (xs, ys) | length xs == v -> 
                                fmap (\x -> (x, ys)) (sequence $ fmap parseVertex xs) :: Either String ([Point3D], [[Char]])
                              otherwise -> Left "Error: Not enough vertices."
    (triangles, rest4)   <- case splitAt f rest3 of
                              (xs, ys) | length xs == f -> 
                                fmap (\x -> (x, ys)) (sequence $ fmap (\x -> parseTriangle x v) xs) :: Either String ([Face3], [[Char]])
                              otherwise -> Left "Error: Not enough faces."
    offContents          <- case rest4 of
                              [] ->        Right (vertices :: [Point3D], triangles :: [Face3])
                              otherwise -> Left "Error: Extra lines found at the end of the file."
    triangles            <- offToSimplicialComplex funcCoord (fst offContents) (snd offContents) 
    return triangles

nextLine :: [[Char]] -> Either String ([Char], [[Char]])
nextLine lines =
  case lines of
    x : xs    -> Right (x, xs)
    otherwise -> Left "Error: Unexpected end of file."

parseFirstLine :: [Char] -> Either String ()
parseFirstLine line =
  case line of
    "OFF"        -> Right ()
    otherwise    -> Left "Error: Expected 'OFF' on line 1."

parseVerticesFacesEdges :: [Char] -> Either String (Int, Int, Int)
parseVerticesFacesEdges line =
  case fmap (\word -> readMaybe word :: Maybe Int) $ words line of
    [Just v, Just f, Just e] -> Right (v, f, e)
    otherwise                -> Left $ "Error: Expected '#vertices #faces #edges' but found '" ++ line ++ "'"

parseVertex :: [Char] -> Either String Point3D
parseVertex line = 
  case fmap (\word -> readMaybe word :: Maybe Double) $ words line of
    [Just x, Just y, Just z] -> Right $ Point3D x y z
    otherwise                -> Left $ "Error: Invalid vertex '" ++ line ++ "'"

parseTriangle :: [Char] -> Int -> Either String Face3
parseTriangle line maxVertexId = 
  case fmap (\word -> readMaybe word :: Maybe Int) $ words line of
    [Just 3, Just p, Just q, Just r] | vertexIndicesValid p q r -> Right $ Face3 p q r
      where 
        vertexIndicesValid :: Int -> Int -> Int -> Bool
        vertexIndicesValid x y z = 
            0 <= x && x < maxVertexId &&
            0 <= y && y < maxVertexId &&
            0 <= z && z < maxVertexId
    otherwise                -> Left $ "Error: Invalid triangle '" ++ line ++ "'"

offToSimplicialComplex :: FunctionCoordinate -> [Point3D] -> [Face3] -> Either String [Triangle]
offToSimplicialComplex funcCoord points faces = 
  let 
    compareVertices :: (Point3D, InputPointId) -> (Point3D, InputPointId) -> Ordering
    compareVertices (p1, i1) (p2, i2) = 
      let
        getFuncVal :: Point3D -> Double
        getFuncVal pt = case funcCoord of
          X -> x pt
          Y -> y pt
          Z -> z pt
      in
        (getFuncVal $ p1, i1) `compare` (getFuncVal $ p2, i2)

    verticesSorted :: [(Point3D, InputPointId)]
    verticesSorted = sortBy compareVertices $ points `zip` (fmap InputPointId [0..])

    verticesSortedWithRanks :: [(Point3D, InputPointId, FunctionValueRank)]
    verticesSortedWithRanks = fmap (\((a, b), c) -> (a, b, c)) $ verticesSorted `zip` (fmap FunctionValueRank [0..])
 
    mkVertex :: (Point3D, InputPointId, FunctionValueRank) -> InputVertex
    mkVertex (_, id, fRank) = InputVertex id fRank

    simplexVertices :: [InputVertex]
    simplexVertices = fmap mkVertex verticesSortedWithRanks

    -- TODO: Don't use a map here because the `lookup` function is partial, whereas we should be able to
    -- guarantee the ability to transform an index into a vertex.  Refactor.
    idxToVertex :: Map.Map Int InputVertex
    idxToVertex =
      let 
        vertexTuples :: [(Int, InputVertex)]
        vertexTuples = fmap (\v -> (iToInt $ vertexId v, v)) simplexVertices
      in
        Map.fromList vertexTuples

    simplexTriangles :: Maybe [Triangle]
    simplexTriangles = 
      let 
        faceToTriangle :: Face3 -> Maybe Triangle
        faceToTriangle (Face3 p q r) =
          do
            pVertex <- Map.lookup p idxToVertex
            qVertex <- Map.lookup q idxToVertex
            rVertex <- Map.lookup r idxToVertex
            return  $  Triangle (Right pVertex) qVertex (Right rVertex)
      in sequence $ fmap faceToTriangle faces

  in
    case simplexTriangles of
      Just triangles -> Right $ sort triangles
      Nothing        -> Left "Error: Unable to convert OFF file to simplicial complex."