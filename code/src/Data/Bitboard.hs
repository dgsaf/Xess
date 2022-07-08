{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Data.Bitboard
Description :
-}
module Data.Bitboard
  ( encodeSquare, encodeSquares, encodeSquaresBy
  , decodeSquares
  , buildWith, buildBy
  , square
  , lineX, lineY, lineU, lineV
  , bitUnion, bitIntersect
  , limits, hasSquare
  , toggleSquare
  ) where

import Data.Square

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Bits
import Data.Foldable
import Data.Functor
import Data.Function
import Data.Word

-- | Square Encoding/Decoding
encodeSquare :: Square -> Word64
encodeSquare sq = bit $ fromEnum sq

encodeSquares :: (Functor t, Foldable t) => t Square -> Word64
encodeSquares sqs = sum $ fmap encodeSquare sqs

encodeSquaresBy :: (Square -> Bool) -> Word64
encodeSquaresBy pred = encodeSquares $ filter pred enumSquares

decodeSquares :: Word64 -> [Square]
decodeSquares w = filter (testBit w . fromEnum) enumSquares

-- | Building Arrays of Bitboards
buildWith :: (IArray a Word64) => (Square -> [Square]) -> a Square Word64
buildWith f = buildSquaresArray (encodeSquares  . f)

buildBy :: (IArray a Word64) => (Square -> Square -> Bool) -> a Square Word64
buildBy pred = buildSquaresArray (\ sq -> encodeSquaresBy (pred sq))

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

-- | Utility
bitUnion :: (Foldable t) => t Word64 -> Word64
bitUnion ws = foldl' (.|.) zeroBits ws

bitIntersect :: (Foldable t) => t Word64 -> Word64
bitIntersect ws = foldl' (.&.) (complement zeroBits) ws

limits :: Word64 -> (Int, Int)
limits w = (countTrailingZeros w, 63 - countLeadingZeros w)

hasSquare :: Word64 -> Square -> Bool
hasSquare w sq = testBit w $ fromEnum sq

toggleSquare :: Word64 -> Square -> Word64
toggleSquare w sq = w `complementBit` fromEnum sq
