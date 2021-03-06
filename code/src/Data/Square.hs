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

  , dispXY, dispX, dispY
  , norm

  , alignedX, alignedY, alignedXY
  , alignedU, alignedV, alignedUV
  , squaresBetween

  , enumSquares
  , buildSquaresArray
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
dispXY :: Square -> Square -> (Int, Int)
dispXY sq sq' = (dispX sq sq', dispY sq sq')

dispX :: Square -> Square -> Int
dispX = (-) `on` (coordX)

dispY :: Square -> Square -> Int
dispY = (-) `on` (coordY)

norm :: Square -> Square -> Int
norm sq sq' = let (x, y) = dispXY sq sq' in (abs x) + (abs y)

-- | Alignment
alignedX :: Square -> Square -> Bool
alignedX = (==) `on` coordX

alignedY :: Square -> Square -> Bool
alignedY = (==) `on` coordY

alignedXY :: Square -> Square -> Bool
alignedXY sq sq' = alignedX sq sq' || alignedY sq sq'

alignedU :: Square -> Square -> Bool
alignedU = (==) `on` coordU

alignedV :: Square -> Square -> Bool
alignedV = (==) `on` coordV

alignedUV :: Square -> Square -> Bool
alignedUV sq sq' = alignedU sq sq' || alignedV sq sq'

squaresBetween :: Square -> Square -> [Square]
squaresBetween sq sq'
  | alignedX sq sq' = fmap toEnum [i, i + 8 .. j]
  | alignedY sq sq' = fmap toEnum [i, i + 1 .. j]
  | alignedU sq sq' = fmap toEnum [i, i + 9 .. j]
  | alignedV sq sq' = fmap toEnum [i, i + 7 .. j]
  | otherwise       = []
  where
    n  = fromEnum sq
    n' = fromEnum sq'
    i = min n n'
    j = max n n'

-- | Utility
enumSquares :: [Square]
enumSquares = range (minBound, maxBound)

buildSquaresArray :: (IArray a e) => (Square -> e) -> a Square e
buildSquaresArray f = listArray (minBound, maxBound) $ fmap f enumSquares
