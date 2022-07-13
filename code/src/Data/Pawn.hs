{- |
Module      : Data.Pawn
Description :
-}
module Data.Pawn
  ( rankFromRear, rankPromote

  , ahead, aheadU, aheadV
  , behind, behindU, behindV
  , forward, forwardU, forwardV, forwardUV
  , backward, backwardU, backwardV, backwardUV

  , stepForward, pushForward

  , fillsForward, fillsBackward
  , spansForward, spansForwardU, spansForwardV
  , spansBackward, spansBackwardU, spansBackwardV
  , spans, spansU, spansV
  ) where

import Data.Bitboard
import Data.Colour
import Data.Piece
import Data.Square

import Data.Array.IArray
import Data.Bits
import Data.Word

-- |
rankFromRear :: Colour -> Int -> Word64
rankFromRear White i = lineY ! (toEnum $ 8 * i)
rankFromRear Black i = lineY ! (toEnum $ 8 * (7 - i))

rankPromote :: Colour -> Word64
rankPromote c = rankFromRear c 7

-- |
ahead :: Colour -> Square -> Square
ahead White = toEnum . (+) 8 . fromEnum
ahead Black = toEnum . (-) 8 . fromEnum

aheadU :: Colour -> Square -> Square
aheadU White = toEnum . (+) 9 . fromEnum
aheadU Black = toEnum . (-) 9 . fromEnum

aheadV :: Colour -> Square -> Square
aheadV White = toEnum . (+) 7 . fromEnum
aheadV Black = toEnum . (-) 7 . fromEnum

behind :: Colour -> Square -> Square
behind c = ahead (opposite c)

behindU :: Colour -> Square -> Square
behindU c = aheadU (opposite c)

behindV :: Colour -> Square -> Square
behindV c = aheadV (opposite c)

-- |
forward :: Colour -> Word64 -> Word64
forward White = (flip shiftL) 8
forward Black = (flip shiftR) 8

forwardU :: Colour -> Word64 -> Word64
forwardU White = (flip shiftL) 9 . (.&.) (complement (lineX ! toEnum 7))
forwardU Black = (flip shiftR) 9 . (.&.) (complement (lineX ! toEnum 0))

forwardV :: Colour -> Word64 -> Word64
forwardV White = (flip shiftL) 7 . (.&.) (complement (lineX ! toEnum 0))
forwardV Black = (flip shiftR) 7 . (.&.) (complement (lineX ! toEnum 7))

forwardUV :: Colour -> Word64 -> Word64
forwardUV c w = forwardU c w .|. forwardV c w

-- |
backward :: Colour -> Word64 -> Word64
backward c = forward (opposite c)

backwardU :: Colour -> Word64 -> Word64
backwardU c = forwardU (opposite c)

backwardV :: Colour -> Word64 -> Word64
backwardV c = forwardV (opposite c)

backwardUV :: Colour -> Word64 -> Word64
backwardUV c w = backwardU c w .|. backwardV c w

-- |
stepForward :: Word64 -> Colour -> Word64 -> Word64
stepForward occ c w = (complement occ) .&. forward c w

pushForward :: Word64 -> Colour -> Word64 -> Word64
pushForward occ c w = rankFromRear c 3 .&. (f . f $ w)
  where
    f = (.&.) (complement occ) . forward c

-- |
fillsForward :: Colour -> Word64 -> Word64
fillsForward c w = f 32 . f 16 . f 8 $ w
  where
    f i w' = w' .|. (shiftL w' i)

fillsBackward :: Colour -> Word64 -> Word64
fillsBackward c w = f 32 . f 16 . f 8 $ w
  where
    f i w' = w' .|. (shiftR w' i)

-- |
spansForward :: Colour -> Word64 -> Word64
spansForward c w = fillsForward c (forward c w)

spansForwardU :: Colour -> Word64 -> Word64
spansForwardU c w = fillsForward c (forwardU c w)

spansForwardV :: Colour -> Word64 -> Word64
spansForwardV c w = fillsForward c (forwardV c w)

spansBackward :: Colour -> Word64 -> Word64
spansBackward c w = fillsBackward c (backward c w)

spansBackwardU :: Colour -> Word64 -> Word64
spansBackwardU c w = spansBackward c (forwardU c w)

spansBackwardV :: Colour -> Word64 -> Word64
spansBackwardV c w = spansBackward c (forwardV c w)

-- |
spans :: Colour -> Word64 -> Word64
spans c w = fillsForward c w .|. fillsBackward c w

spansU :: Colour -> Word64 -> Word64
spansU c w = spansForwardU c w .|. spansBackwardU c w

spansV :: Colour -> Word64 -> Word64
spansV c w = spansForwardV c w .|. spansBackwardV c w

-- |
-- interspan :: Board -> Word64
-- interspan b = sf White .&. sf Black
--   where
--     sf c = spansForward c (piece b (c, P))

-- isolated :: Board -> Colour -> Word64
-- isolated b c = w .&. complement (spansU c w .|. spansV c w)
--   where
--     w = piece b (c, P)

-- isolatedU :: Board -> Colour -> Word64
-- isolatedU b c = w .&. complement (spansU c w)
--   where
--     w = piece b (c, P)

-- isolatedV :: Board -> Colour -> Word64
-- isolatedV b c = w .&. complement (spansV c w)
--   where
--     w = piece b (c, P)

-- doubledForward :: Board -> Colour -> Word64
-- doubledForward b c = w .&. spansForward c w
--   where
--     w = piece b (c, P)

-- doubledBackward :: Board -> Colour -> Word64
-- doubledBackward b c = w .&. spansBackward c w
--   where
--     w = piece b (c, P)
