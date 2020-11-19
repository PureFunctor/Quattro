module Quattro where

import Data.Map.Strict ()
import Data.Map.Strict as M
import Data.List (elemIndices, transpose)

type Coordinates = (Int, Int)

type Matrix = Map Coordinates

data Shape = J | L | I | O | S | Z | T
    deriving (Eq, Show)

data Rotation = R0 | R1 | R2 | R3
    deriving (Enum, Eq, Show)

data Block
    = Block
    { shape       :: Shape
    , coordinates :: [Coordinates]
    , rotation    :: Rotation
    }
    deriving (Eq, Show)

data Field
    = Field
    { field   :: Matrix Block
    , bounds  :: Coordinates
    , current :: Block
    }
    deriving (Eq, Show)

succR :: Rotation -> Rotation
succR r = case fromEnum r of
    3 -> R0
    n -> toEnum $ (n + 1) `mod` 4

prevR :: Rotation -> Rotation
prevR r = case fromEnum r of
    0 -> R3
    n -> toEnum . abs $ (n - 1) `mod` 4

rotate90 :: [[a]] -> [[a]]
rotate90 = transpose . reverse

inStrings :: Shape -> [String]
inStrings J = ["x  ", "xxx", "   "]
inStrings L = reverse <$> inStrings J
inStrings I = ["    ", "xxxx", "    ", "    "]
inStrings O = [" xx ", " xx ", "    "]
inStrings S = [" xx", "xx ", "   "]
inStrings Z = reverse <$> inStrings S
inStrings T = [" x ", "xxx", "   "]

inCoordinates :: [String] -> [Coordinates]
inCoordinates = concat . zipWith (zip . repeat) [0 ..] . (elemIndices 'x' <$>)
