module Quattro where

import Data.Map.Strict()
import Data.Map.Strict as M

type Coordinates = (Int, Int)
type Matrix = Map Coordinates 

data Shape = J | L | I | O | S | Z | T deriving (Eq, Show)
data Rotation = R0| R1 | R2 | R3 deriving (Enum, Eq, Show)
data Block = Block { shape :: Shape, coordinates :: [Coordinates], rotation :: Rotation} deriving (Eq, Show)
data Field = Field { field :: Matrix Block, bounds :: Coordinates, current :: Block} deriving (Eq, Show)

succR :: Rotation -> Rotation
succR r = case fromEnum r of
                   3 -> R0
                   _ -> toEnum $ (fromEnum r + 1) `mod` 3

prevR :: Rotation -> Rotation
prevR r = case fromEnum r of
                   0 -> R3
                   _ -> toEnum . abs $ (fromEnum r - 1) `mod` 3
