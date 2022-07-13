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
  , shiftX, shiftY, shiftXY, shiftsXY
  , bitUnion, bitIntersect
  , limits, hasSquare
  , toggleSquare
  , lineBetween
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
-- decodeSquares w = filter (testBit w . fromEnum) enumSquares
decodeSquares w = fmap toEnum $ f 0 w
  where
    f i w'
      | t == 64   = []
      | otherwise = (i + t) : f (i + t + 1) (shiftR w' (t + 1))
      where
        t = countTrailingZeros w'

-- | Building Arrays of Bitboards
buildWith :: (IArray a Word64) => (Square -> [Square]) -> a Square Word64
buildWith f = buildSquaresArray (encodeSquares  . f)

buildBy :: (IArray a Word64) => (Square -> Square -> Bool) -> a Square Word64
buildBy pred = buildSquaresArray (\ sq -> encodeSquaresBy (pred sq))

-- | Primitive Bitboards
-- Note that `lineZ` means a line with constant Z, not a line parallel to the Z
-- axis.
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

-- | Shift
shiftX :: Word64 -> Int -> Word64
shiftX w i = bitUnion $ fmap f [0 .. 7]
  where
    f y = let l = lineY ! toEnum (8 * y) in ((w .&. l) `shift` i) .&. l

shiftY :: Word64 -> Int -> Word64
shiftY w j = w `shift` (8*j)

shiftXY :: Word64 -> (Int, Int) -> Word64
shiftXY w (i, j) = (w `shiftX` i) `shiftY` j

shiftsXY :: (Functor t, Foldable t) => Word64 -> t (Int, Int) -> Word64
shiftsXY w shs = bitUnion $ fmap (shiftXY w) shs

-- | Combine Bitboards
bitUnion :: (Foldable t) => t Word64 -> Word64
bitUnion ws = foldl' (.|.) zeroBits ws

bitIntersect :: (Foldable t) => t Word64 -> Word64
bitIntersect ws = foldl' (.&.) (complement zeroBits) ws

-- | Utility
limits :: Word64 -> (Int, Int)
limits w = (countTrailingZeros w, 63 - countLeadingZeros w)

hasSquare :: Word64 -> Square -> Bool
hasSquare w sq = testBit w $ fromEnum sq

toggleSquare :: Word64 -> Square -> Word64
toggleSquare w sq = w `complementBit` fromEnum sq

lineBetween :: Square -> Square -> Word64
lineBetween sq sq' = encodeSquares $ squaresBetween sq sq'
