module Off where

import Text.Read

{-
TODO:  This is an extremely rudimentary, and frankly extremely crappy, parser.
       It's temporary.

       Obviously we want to use the state monad here to trundle around the 'restX' values.
       But not today, it can wait.

       And the return value should be a record rather than a tuple.
-}

parseOffFile :: [[Char]] -> Either String ([(Double, Double, Double)], [(Int, Int, Int)])
parseOffFile lines = 
  do
    (line1, rest1)       <- nextLine lines
    _                    <- parseFirstLine line1
    (line2, rest2)       <- nextLine rest1
    (v, f, e)            <- parseVerticesFacesEdges line2
    (vertices, rest3)    <- case splitAt v rest2 of
                              (xs, ys) | length xs == v -> 
                                fmap (\x -> (x, ys)) (sequence $ fmap parseVertex xs) :: Either String ([(Double, Double, Double)], [[Char]])
                              otherwise -> Left "Error: Not enough vertices."
    (triangles, rest4)   <- case splitAt f rest3 of
                              (xs, ys) | length xs == f -> 
                                fmap (\x -> (x, ys)) (sequence $ fmap (\x -> parseTriangle x v) xs) :: Either String ([(Int, Int, Int)], [[Char]])
                              otherwise -> Left "Error: Not enough faces."
    result               <- case rest4 of
                              [] ->        Right (vertices :: [(Double, Double, Double)], triangles :: [(Int, Int, Int)])
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

parseVertex :: [Char] -> Either String (Double, Double, Double)
parseVertex line = 
  case fmap (\word -> readMaybe word :: Maybe Double) $ words line of
    [Just x, Just y, Just z] -> Right (x, y, z)
    otherwise                -> Left $ "Error: Invalid vertex '" ++ line ++ "'"

parseTriangle :: [Char] -> Int -> Either String (Int, Int, Int)
parseTriangle line maxVertexId = 
  case fmap (\word -> readMaybe word :: Maybe Int) $ words line of
    [Just 3, Just x, Just y, Just z] | vertexIndicesValid x y z -> Right (x, y, z)
      where 
        vertexIndicesValid :: Int -> Int -> Int -> Bool
        vertexIndicesValid x y z = 
            0 <= x && x < maxVertexId &&
            0 <= y && y < maxVertexId &&
            0 <= z && z < maxVertexId
    otherwise                -> Left $ "Error: Invalid triangle '" ++ line ++ "'"