{- |
Module      : Data.Bitboard
Description :
-}
module Data.Bitboard
  ( encodeSquare, encodeSquares, encodeSquaresBy
  , decodeSquares
  , build, buildWith, buildBy
  , square
  , lineX, lineY, lineU, lineV
  , bitUnion, bitIntersect
  , limits, hasSquare
  ) where

import Data.Square

import Data.Array.Unboxed
import Data.Bits
import Data.Foldable
import Data.Functor
import Data.Function
import Data.Word

-- | Square Encoding/Decoding
encodeSquare :: Square -> Word64
encodeSquare sq = 2 ^ fromEnum sq

encodeSquares :: (Functor t, Foldable t) => t Square -> Word64
encodeSquares sqs = sum $ fmap encodeSquare sqs

encodeSquaresBy :: (Square -> Bool) -> Word64
encodeSquaresBy pred = encodeSquares $ filter pred squaresList

decodeSquares :: Word64 -> [Square]
decodeSquares w = filter (testBit w . fromEnum) squaresList

-- | Building Arrays of Bitboards
build :: (Square -> Word64) -> UArray Square Word64
build f = listArray (minBound, maxBound) $ fmap f squaresList

buildWith :: (Square -> [Square]) -> UArray Square Word64
buildWith f = build (encodeSquares  . f)

buildBy :: (Square -> Square -> Bool) -> UArray Square Word64
buildBy pred = build (\ sq -> encodeSquaresBy (pred sq))

-- | Primitive Bitboards
square :: UArray Square Word64
square = buildBy ((==) `on` coordXY)

lineX :: UArray Square Word64
lineX = buildBy ((==) `on` coordX)

lineY :: UArray Square Word64
lineY = buildBy ((==) `on` coordY)

lineU :: UArray Square Word64
lineU = buildBy ((==) `on` coordU)

lineV :: UArray Square Word64
lineV = buildBy ((==) `on` coordV)

-- |
bitUnion :: (Foldable t) => t Word64 -> Word64
bitUnion ws = foldl' (.|.) zeroBits ws

bitIntersect :: (Foldable t) => t Word64 -> Word64
bitIntersect ws = foldl' (.&.) (complement zeroBits) ws

-- | Utility
limits :: Word64 -> (Int, Int)
limits w = (countTrailingZeros w, 63 - countLeadingZeros w)

hasSquare :: Word64 -> Square -> Bool
hasSquare w sq = testBit w $ fromEnum sq
