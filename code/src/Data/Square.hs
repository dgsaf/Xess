{- |
Module      : Data.Square
Description :
-}
module Data.Square
  ( Square
  , mkSquare, mkSquareXY

  , coordXY, coordX, coordY
  , coordUV, coordU, coordV

  , translateXY, translationsXY

  , distXY, distX, distY

  , enumSquares
  , buildSquaresArray
  , collinear
  ) where

import Data.Array.IArray
import Data.Foldable
import Data.Function
import Data.Ix
import Data.Maybe
import Data.Tuple

-- | Square
newtype Square
  = Square Int
  deriving (Eq, Ord, Ix, Read, Show)

instance Bounded Square where
  minBound = Square 0
  maxBound = Square 63

instance Enum Square where
  fromEnum (Square n) = n
  toEnum n
    | inRange (0, 63) n = Square n
    | otherwise         = undefined

-- | Smart Constructors
mkSquare :: Int -> Maybe Square
mkSquare n
  | inRange (0, 63) n = Just $ Square n
  | otherwise         = Nothing

mkSquareXY :: (Int, Int) -> Maybe Square
mkSquareXY (x, y)
  | inRange ((0, 0), (7, 7)) (x, y) = mkSquare $ (8*y + x)
  | otherwise                       = Nothing

-- | Coordinates
coordXY :: Square -> (Int, Int)
coordXY (Square n) = swap $ n `quotRem` 8

coordX :: Square -> Int
coordX = fst . coordXY

coordY :: Square -> Int
coordY = snd . coordXY

coordUV :: Square -> (Int, Int)
coordUV sq = (7 + x - y, x + y)
  where
    (x, y) = coordXY sq

coordU :: Square -> Int
coordU = fst . coordUV

coordV :: Square -> Int
coordV = snd . coordUV

-- | Coordinate Translation
translateXY :: (Int, Int) -> Square -> Maybe Square
translateXY (i, j) sq = mkSquareXY $ (x + i, y + j)
  where
    (x, y) = coordXY sq

translationsXY :: [(Int, Int)] -> Square -> [Square]
translationsXY ts sq = mapMaybe (\t -> translateXY t sq) ts

-- | Coordinate Distance
distXY :: Square -> Square -> (Int, Int)
distXY sq sq' = (distX sq sq', distY sq sq')

distX :: Square -> Square -> Int
distX = (-) `on` (coordX)

distY :: Square -> Square -> Int
distY = (-) `on` (coordY)

-- | Utility
enumSquares :: [Square]
enumSquares = range (minBound, maxBound)

buildSquaresArray :: (IArray a e) => (Square -> e) -> a Square e
buildSquaresArray f = listArray (minBound, maxBound) $ fmap f enumSquares

collinear :: [Square] -> Bool
collinear []  = False
collinear sqs = any (\ f -> all (\ sq' -> f (head sqs) == f sq') sqs)
                $ [coordX, coordY, coordU, coordV]
