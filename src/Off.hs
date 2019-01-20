module Off where

import Text.Read
import Simplex as S

{-
TODO:  This is an extremely rudimentary, and frankly extremely crappy, parser.
       It's temporary.

       Obviously we want to use the state monad here to trundle around the 'restX' values.
       But not today, it can wait.

       And the return value should be a record rather than a tuple.
-}

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


parseOffFile :: [[Char]] -> Either String ([Point3D], [Face3])
parseOffFile lines = 
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
    result               <- case rest4 of
                              [] ->        Right (vertices :: [Point3D], triangles :: [Face3])
                              otherwise -> Left "Error: Extra lines found at the end of the file."

    return result

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