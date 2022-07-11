{- |
Module      : Data.Rotated
Description :
-}
module Data.Rotated
  ( RotWord64
  , mkRotWord64
  , view
  , rotToggleSquare
  , zeroRotWord64, mkRotWord64Toggle
  , visibleX, visibleY, visibleU, visibleV
  ) where

import Data.Bitboard
import Data.Square

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Bits
import Data.Ix
import Data.Function
import Data.List
import Data.Ord
import Data.Word

-- | Rotation
data Rot
  = RotX
  | RotY
  | RotU
  | RotV
  deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

ordering :: Rot -> (Square -> Square -> Ordering)
ordering RotX = comparing (\sq -> let (x, y) = coordXY sq in (y, x))
ordering RotY = comparing (\sq -> let (x, y) = coordXY sq in (x, -y))
ordering RotU = comparing (\sq -> let (u, v) = coordUV sq in (v, u))
ordering RotV = comparing (\sq -> let (u, v) = coordUV sq in (-u, v))

enumRotSquares :: Rot -> [Square]
enumRotSquares r = sortBy (ordering r) enumSquares

rotIx :: Rot -> Array Square Square
rotIx = array (minBound, maxBound) . (flip zip) enumSquares . enumRotSquares

rotInvIx :: Rot -> Array Square Square
rotInvIx = array (minBound, maxBound) . zip enumSquares . enumRotSquares

rotation :: Rot -> Word64 -> Word64
rotation r w = encodeSquares $ fmap (rotIx r !) $ decodeSquares w

invRotation :: Rot -> Word64 -> Word64
invRotation r w = encodeSquares $ fmap (rotInvIx r !) $ decodeSquares w

-- |
newtype RotWord64
  = RotWord64 (Word64, Word64, Word64, Word64)
  deriving (Eq, Ord, Read, Show)

-- | Constructors
mkRotWord64 :: Word64 -> RotWord64
mkRotWord64 w =
  RotWord64
  ( rotation RotX w
  , rotation RotY w
  , rotation RotU w
  , rotation RotV w)

-- | View Rotated Bitboards
(!>) :: RotWord64 -> Rot -> Word64
(!>) (RotWord64 (wX, wY, wU, wV)) RotX = wX
(!>) (RotWord64 (wX, wY, wU, wV)) RotY = wY
(!>) (RotWord64 (wX, wY, wU, wV)) RotU = wU
(!>) (RotWord64 (wX, wY, wU, wV)) RotV = wV

view :: RotWord64 -> Word64
view rw = rw !> RotX

-- | Incremental Update
rotToggleSquare :: RotWord64 -> Square -> RotWord64
rotToggleSquare rw sq = RotWord64 (f RotX, f RotY, f RotU, f RotV)
  where
    f r = toggleSquare (rw !> r) (rotIx r ! sq)

-- | Incremental Constructor
zeroRotWord64 :: RotWord64
zeroRotWord64 = RotWord64 (zeroBits, zeroBits, zeroBits, zeroBits)

mkRotWord64Toggle :: Word64 -> RotWord64
mkRotWord64Toggle w = foldl' rotToggleSquare zeroRotWord64 $ decodeSquares w

-- | Primitive Rotated Bitboards
rotSquare :: Array Square RotWord64
rotSquare = amap mkRotWord64 $ buildBy ((==) `on` coordXY)

rotLineX :: Array Square RotWord64
rotLineX = amap mkRotWord64 $ buildBy ((==) `on` coordX)

rotLineY :: Array Square RotWord64
rotLineY = amap mkRotWord64 $ buildBy ((==) `on` coordY)

rotLineU :: Array Square RotWord64
rotLineU = amap mkRotWord64 $ buildBy ((==) `on` coordU)

rotLineV :: Array Square RotWord64
rotLineV = amap mkRotWord64 $ buildBy ((==) `on` coordV)

rotLine :: Rot -> Array Square Word64
rotLine RotX = fmap (!> RotX) rotLineY
rotLine RotY = fmap (!> RotY) rotLineX
rotLine RotU = fmap (!> RotU) rotLineV
rotLine RotV = fmap (!> RotV) rotLineU

-- | Occupancy
trim :: Word8 -> Word8
trim occ = occ `clearBit` 7 `shiftR` 1

untrim :: Word8 -> Word8
untrim occ = (occ `shiftL` 1) `setBit` 0 `setBit` 7

firstL :: Word8 -> Int -> Int
firstL w k = maybe 7 ((+) k) $ find (\i -> testBit (shiftR w i) k) [1 .. (7-k)]

firstR :: Word8 -> Int -> Int
firstR w k = maybe 0 ((-) k) $ find (\i -> testBit (shiftL w i) k) [1 .. k]

occupancy :: RotWord64 -> Square -> Rot -> Word8
occupancy rw sq r = fromInteger . toInteger $ ((rw !> r) .&. line) `shiftR` a
  where
    line = rotLine r ! sq
    (a, b) = limits line

visible :: Rot -> (Square, Word8) -> Word64
visible r (sq, occ) = lineBetween sqR sqL
  where
    line = rotLine r ! sq
    (a, b) = limits line
    k = (fromEnum $ rotIx r ! sq) - a
    i = a + firstR occ k
    j = min (a + firstL occ k) b
    (sqR, sqL) = (rotInvIx r ! toEnum i, rotInvIx r ! toEnum j)

-- | Primitive Occupancy Bitboards
buildVisibleLine :: Rot -> UArray (Square, Word8) Word64
buildVisibleLine r = listArray bs $ fmap f $ range bs
  where
    bs = ((minBound, fromInteger 0), (maxBound, fromInteger 63))
    f (sq, occTrim) = visible r (sq, untrim occTrim)

visibleLineX :: UArray (Square, Word8) Word64
visibleLineX = buildVisibleLine RotY

visibleLineY :: UArray (Square, Word8) Word64
visibleLineY = buildVisibleLine RotX

visibleLineU :: UArray (Square, Word8) Word64
visibleLineU = buildVisibleLine RotV

visibleLineV :: UArray (Square, Word8) Word64
visibleLineV = buildVisibleLine RotU

-- | Visibility Bitboards
visibleX :: RotWord64 -> Square -> Word64
visibleX rw sq = visibleLineX ! (sq, trim $ occupancy rw sq RotY)

visibleY :: RotWord64 -> Square -> Word64
visibleY rw sq = visibleLineY ! (sq, trim $ occupancy rw sq RotX)

visibleU :: RotWord64 -> Square -> Word64
visibleU rw sq = visibleLineU ! (sq, trim $ occupancy rw sq RotV)

visibleV :: RotWord64 -> Square -> Word64
visibleV rw sq = visibleLineV ! (sq, trim $ occupancy rw sq RotU)
