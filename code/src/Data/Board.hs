{- |
Module      : Data.Board
Description :
-}

module Data.Board
  ( Board
  , emptyBoard, defaultBoard
  , toggle, insertPiece, removePiece, clearSquare
  , piece, colour, occupied
  , visibleB, visibleR, visibleQ
  , hostile, engageable, unoccupied
  , pieceAt, (!?), locateKing
  ) where

import Data.Bitboard
import Data.Colour
import Data.Piece
import Data.Rotated
import Data.Square

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Bits
import Data.Ix
import Data.List
import Data.Maybe
import Data.Word

-- | Board
data Board
  = Board
  { _whiteP :: Word64
  , _whiteN :: Word64
  , _whiteB :: Word64
  , _whiteR :: Word64
  , _whiteQ :: Word64
  , _whiteK :: Word64
  , _blackP :: Word64
  , _blackN :: Word64
  , _blackB :: Word64
  , _blackR :: Word64
  , _blackQ :: Word64
  , _blackK :: Word64
  , _white :: Word64
  , _black :: Word64
  , _rotOccupied :: RotWord64
  } deriving (Eq, Ord, Read, Show)

-- | Constructors
emptyBoard :: Board
emptyBoard =
  Board
  { _whiteP = zeroBits
  , _whiteN = zeroBits
  , _whiteB = zeroBits
  , _whiteR = zeroBits
  , _whiteQ = zeroBits
  , _whiteK = zeroBits
  , _blackP = zeroBits
  , _blackN = zeroBits
  , _blackB = zeroBits
  , _blackR = zeroBits
  , _blackQ = zeroBits
  , _blackK = zeroBits
  , _white = zeroBits
  , _black = zeroBits
  , _rotOccupied = mkRotWord64 zeroBits
  }

defaultBoard :: Board
defaultBoard =
  Board
  { _whiteP = wp
  , _whiteN = wn
  , _whiteB = wb
  , _whiteR = wr
  , _whiteQ = wq
  , _whiteK = wk
  , _blackP = bp
  , _blackN = bn
  , _blackB = bb
  , _blackR = br
  , _blackQ = bq
  , _blackK = bk
  , _white = w
  , _black = b
  , _rotOccupied = mkRotWord64 occ
  }
  where
    wp = lineX ! toEnum 8
    wn = (square ! toEnum 1) .|. (square ! toEnum 6)
    wb = (square ! toEnum 2) .|. (square ! toEnum 5)
    wr = (square ! toEnum 0) .|. (square ! toEnum 7)
    wq = square ! toEnum 3
    wk = square ! toEnum 4
    bp = wp `shiftL` 40
    bn = wn `shiftL` 56
    bb = wb `shiftL` 56
    br = wr `shiftL` 56
    bq = wq `shiftL` 56
    bk = wk `shiftL` 56
    w = bitUnion [wp, wn, wb, wr, wq, wk]
    b = bitUnion [bp, bn, bb, br, bq, bk]
    occ = w .|. b

-- | Incremental Update
toggle :: (Colour, Piece) -> Square -> Board -> Board
toggle (c, p) sq b =
  toggleOccupied sq . toggleColour c sq . togglePiece (c, p) sq $ b

togglePiece :: (Colour, Piece) -> Square -> Board -> Board
togglePiece (White, P) sq b = b {_whiteP = toggleSquare (_whiteP b) sq}
togglePiece (White, N) sq b = b {_whiteN = toggleSquare (_whiteN b) sq}
togglePiece (White, B) sq b = b {_whiteB = toggleSquare (_whiteB b) sq}
togglePiece (White, R) sq b = b {_whiteR = toggleSquare (_whiteR b) sq}
togglePiece (White, Q) sq b = b {_whiteQ = toggleSquare (_whiteQ b) sq}
togglePiece (White, K) sq b = b {_whiteK = toggleSquare (_whiteK b) sq}
togglePiece (Black, P) sq b = b {_blackP = toggleSquare (_blackP b) sq}
togglePiece (Black, N) sq b = b {_blackN = toggleSquare (_blackN b) sq}
togglePiece (Black, B) sq b = b {_blackB = toggleSquare (_blackB b) sq}
togglePiece (Black, R) sq b = b {_blackR = toggleSquare (_blackR b) sq}
togglePiece (Black, Q) sq b = b {_blackQ = toggleSquare (_blackQ b) sq}
togglePiece (Black, K) sq b = b {_blackK = toggleSquare (_blackK b) sq}

toggleColour :: Colour -> Square -> Board -> Board
toggleColour White sq b = b {_white = toggleSquare (_white b) sq}
toggleColour Black sq b = b {_black = toggleSquare (_black b) sq}

toggleOccupied :: Square -> Board -> Board
toggleOccupied sq b = b {_rotOccupied = rotToggleSquare (_rotOccupied b) sq}

insertPiece :: (Colour, Piece) -> Square -> Board -> Board
insertPiece (c, p) sq b
  | isNothing $ b !? sq = toggle (c, p) sq b
  | otherwise           = undefined

removePiece :: (Colour, Piece) -> Square -> Board -> Board
removePiece (c, p) sq b
  | maybe False ((==) (c, p)) (b !? sq) = toggle (c, p) sq b
  | otherwise                           = undefined

clearSquare :: Square -> Board -> Board
clearSquare sq b = maybe b (\ (c, p) -> toggle (c, p) sq b) (b !? sq)

-- | Properties
piece :: Board -> (Colour, Piece) -> Word64
piece b (White, P) = _whiteP b
piece b (White, N) = _whiteN b
piece b (White, B) = _whiteB b
piece b (White, R) = _whiteR b
piece b (White, Q) = _whiteQ b
piece b (White, K) = _whiteK b
piece b (Black, P) = _blackP b
piece b (Black, N) = _blackN b
piece b (Black, B) = _blackB b
piece b (Black, R) = _blackR b
piece b (Black, Q) = _blackQ b
piece b (Black, K) = _blackK b

colour :: Board -> Colour -> Word64
colour b White = _white b
colour b Black = _black b

occupied :: Board -> Word64
occupied b = view $ _rotOccupied b

-- | Visibility
visibleB :: Board -> Square -> Word64
visibleB b sq = (visibleU rOcc sq .|. visibleV rOcc sq) `clearBit` fromEnum sq
  where
    rOcc = _rotOccupied b

visibleR :: Board -> Square -> Word64
visibleR b sq = (visibleX rOcc sq .|. visibleY rOcc sq) `clearBit` fromEnum sq
  where
    rOcc = _rotOccupied b

visibleQ :: Board -> Square -> Word64
visibleQ b sq = visibleB b sq .|. visibleR b sq

-- | Derived Properties
hostile :: Board -> Colour -> Word64
hostile b c = colour b (opposite c)

engageable :: Board -> Colour -> Word64
engageable b c = complement $ colour b c

unoccupied :: Board -> Word64
unoccupied b = complement $ occupied b

-- |
pieceAt :: Board -> Square -> Maybe (Colour, Piece)
pieceAt b sq = find (\ (c, p) -> hasSquare (piece b (c, p)) sq)
               $ range (minBound, maxBound)

(!?) :: Board -> Square -> Maybe (Colour, Piece)
(!?) b sq = pieceAt b sq

locateKing :: Board -> Colour -> Square
locateKing b c
  | n /= 64   = toEnum n
  | otherwise = undefined
  where
    n = countTrailingZeros $ piece b (c, K)

-- | Scope

-- | Attack

-- | Xray

-- | Move

-- | Check, Pin

-- | Static

-- | Slide

-- | Representations
