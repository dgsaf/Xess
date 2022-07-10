{- |
Module      : Data.Move
Description :
-}
module Data.Move
  (
  ) where

import Data.Castle
import Data.Bitboard
import Data.Board
import Data.Colour
import Data.Piece
import Data.Square

import Data.Array.IArray
import Data.Bits
import Data.Ix
import Data.Maybe
import Data.Word

-- | Encoded Capture Types
data CaptureT
  = CaptureP
  | CaptureN
  | CaptureB
  | CaptureR
  | CaptureQ
  | CaptureEP
  deriving (Eq, Ord, Ix, Enum, Bounded, Read, Show)

encodeCaptureT :: Piece -> CaptureT
encodeCaptureT P = CaptureP
encodeCaptureT N = CaptureN
encodeCaptureT B = CaptureB
encodeCaptureT R = CaptureR
encodeCaptureT Q = CaptureQ

decodeCaptureT :: CaptureT -> Piece
decodeCaptureT CaptureP  = P
decodeCaptureT CaptureN  = N
decodeCaptureT CaptureB  = B
decodeCaptureT CaptureR  = R
decodeCaptureT CaptureQ  = Q
decodeCaptureT CaptureEP = P

squaresEP :: Square -> Maybe (Square, Square)
squaresEP sqEP
  | y == 2    = Just (toEnum $ n - 8, toEnum $ n + 8)
  | y == 5    = Just (toEnum $ n + 8, toEnum $ n - 8)
  | otherwise = Nothing
  where
    y = coordY sqEP
    n = fromEnum sqEP

originCapture :: Square -> CaptureT -> Square
originCapture sq CaptureEP = snd . fromJust . squaresEP $ sq
originCapture sq _         = sq

-- | Encoded Promotion Types
data PromoteT
  = PromoteN
  | PromoteB
  | PromoteR
  | PromoteQ
  deriving (Eq, Ord, Ix, Enum, Bounded, Read, Show)

decodePromoteT :: PromoteT -> Piece
decodePromoteT PromoteN = N
decodePromoteT PromoteB = B
decodePromoteT PromoteR = R
decodePromoteT PromoteQ = Q

promoteMask :: Word64
promoteMask = (lineY ! (toEnum 0)) .|. (lineY ! (toEnum 56))

-- | Move
data Move
  = Normal (Square, Piece) (Square, Maybe CaptureT) (Maybe PromoteT)
  | Castle (Colour, Side)
  deriving (Eq, Read, Show)

origin :: Move -> Square
origin (Normal (sq, p) (sq', mct) (mpt)) = sq
origin (Castle (c, s)) = fst . castleSquaresK $ (c, s)

target :: Move -> Square
target (Normal (sq, p) (sq', mct) (mpt)) = sq'
target (Castle (c, s)) = snd . castleSquaresK $ (c, s)

moving :: Move -> Piece
moving (Normal (sq, p) (sq', mct) (mpt)) = p
moving (Castle (c, s)) = K

capturing :: Move -> Maybe Piece
capturing (Normal (sq, p) (sq', mct) (mpt)) = fmap decodeCaptureT mct
capturing (Castle (c, s)) = Nothing

promotingTo :: Move -> Maybe Piece
promotingTo (Normal (sq, p) (sq', mct) (mpt)) = fmap decodePromoteT mpt
promotingTo (Castle (c, s)) = Nothing

-- | Constructor
quiet :: (Square, Piece) -> Square -> Move
quiet (sq, p) sq' = Normal (sq, p) (sq', Nothing) Nothing

quietPromote :: (Square, Piece) -> Square -> PromoteT -> Move
quietPromote (sq, p) sq' pt = Normal (sq, p) (sq', Nothing) (Just pt)

capture :: (Square, Piece) -> (Square, CaptureT) -> Move
capture (sq, p) (sq', ct) = Normal (sq, p) (sq', Just ct) Nothing

capturePromote :: (Square, Piece) -> (Square, CaptureT) -> PromoteT -> Move
capturePromote (sq, p) (sq', ct) pt = Normal (sq, p) (sq', Just ct) (Just pt)

castle :: (Colour, Side) -> Move
castle (c, s) = Castle (c, s)

-- |
quietMoves :: Board -> Square -> (Colour, Piece) -> Word64 -> [Move]
quietMoves b sq (c, P) moveMask = qs ++ concat (fmap qps [minBound .. maxBound])
  where
    maskQ  = moveMask .&. unoccupied b .&. (complement promoteMask)
    maskQP = moveMask .&. unoccupied b .&. promoteMask
    qs     = fmap (quiet (sq, P)) $ decodeSquares maskQ
    qps pt = fmap (\ sq' -> quietPromote (sq, P) sq' pt) $ decodeSquares maskQP
quietMoves b sq (c, p) moveMask = qs
  where
    maskQ = moveMask .&. unoccupied b
    qs = fmap (quiet (sq, p)) $ decodeSquares maskQ

captureMoves :: Board -> Square -> (Colour, Piece) -> Word64 -> [Move]
captureMoves b sq (c, P) moveMask = cs ++ concat (fmap cps [minBound .. maxBound])
  where
    maskC  = moveMask .&. hostile b c .&. (complement promoteMask)
    maskCP = moveMask .&. hostile b c .&. promoteMask
    ct sq' = encodeCaptureT . snd . fromJust $ b !? sq'
    cs     = fmap (\ sq' -> capture (sq, P) (sq', ct sq'))
             $ decodeSquares maskC
    cps pt = fmap (\ sq' -> capturePromote (sq, P) (sq', ct sq') pt)
             $ decodeSquares maskC
captureMoves b sq (c, p) moveMask = cs
  where
    maskC = moveMask .&. hostile b c
    ct sq' = encodeCaptureT . snd . fromJust $ b !? sq'
    cs = fmap (\ sq' -> capture (sq, p) (sq', ct sq')) $ decodeSquares maskC

enPassantMoves :: Board -> Square -> (Colour, Piece) -> Word64 -> Square -> [Move]
enPassantMoves b sq (c, P) moveMask sqEP
  | maskEP /= zeroBits = [capture (sq, P) (sqEP, CaptureEP)]
  | otherwise          = []
  where
    maskEP = moveMask .&. scopeEP b (c, p) sqEP
enPassantMoves b sq (c, _) moveMask sqEP = []
