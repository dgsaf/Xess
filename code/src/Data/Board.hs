{- |
Module      : Data.Board
Description :
-}

module Data.Board
  ( Board
  , emptyBoard, defaultBoard
  , toggle, insertPiece, removePiece, clearSquare

  , piece, colour, occupied
  , slideX, slideY, slideU, slideV
  , hostile, engageable, unoccupied
  , pieceAt, (!?), locateKing

  , active
  , attacksFrom, attacksTo, attacking, attacked
  , sightedX, sightedY, sightedU, sightedV
  , xrayX, xrayY, xrayU, xrayV
  , scope
  , checks, inCheck

  , quietP
  , activeP, activeN, activeK
  , activeB, activeR, activeQ

  , toPieceList, fromPieceList
  , toArray, fromArray
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
    wp = lineY ! toEnum 8
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

-- | Sliding
slideX :: Board -> Square -> Word64
slideX b sq = visibleX (_rotOccupied b) sq `clearBit` fromEnum sq

slideY :: Board -> Square -> Word64
slideY b sq = visibleY (_rotOccupied b) sq `clearBit` fromEnum sq

slideU :: Board -> Square -> Word64
slideU b sq = visibleU (_rotOccupied b) sq `clearBit` fromEnum sq

slideV :: Board -> Square -> Word64
slideV b sq = visibleV (_rotOccupied b) sq `clearBit` fromEnum sq

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

-- | Attack
active :: Board -> Square -> (Colour, Piece) -> Word64
active b sq (c, P) = activeP c ! sq
active b sq (c, N) = activeN ! sq
active b sq (c, B) = activeB b sq
active b sq (c, R) = activeR b sq
active b sq (c, Q) = activeQ b sq
active b sq (c, K) = activeK ! sq

attacksFrom :: Board -> Square -> Word64
attacksFrom b sq = maybe zeroBits (active b sq) (b !? sq)

attacksTo :: Board -> Square -> Word64
attacksTo b sq = encodeSquaresBy (\ sq' -> hasSquare (attacksFrom b sq') sq)

attacking :: Board -> Colour -> Word64
attacking b c = bitUnion $ fmap (attacksFrom b) $ decodeSquares $ colour b c

attacked :: Board -> Colour -> Word64
attacked b c = attacking b $ opposite c

-- | Xray
sightedX :: Board -> Square -> [Square]
sightedX b sq = decodeSquares $ slideX b sq .&. occupied b

sightedY :: Board -> Square -> [Square]
sightedY b sq = decodeSquares $ slideY b sq .&. occupied b

sightedU :: Board -> Square -> [Square]
sightedU b sq = decodeSquares $ slideU b sq .&. occupied b

sightedV :: Board -> Square -> [Square]
sightedV b sq = decodeSquares $ slideV b sq .&. occupied b

xrayWith :: Board -> Square -> (Board -> Square -> [Square]) -> [(Square, Square)]
xrayWith b sq sight
  = concat $ fmap (\ sq' -> zip (repeat sq') $ g sq') $ sight b sq
  where
    g sq' = filter (\ sq'' -> compare sq sq' == compare sq' sq'') $ sight b sq'

xrayX :: Board -> Square -> [(Square, Square)]
xrayX b sq = xrayWith b sq sightedX

xrayY :: Board -> Square -> [(Square, Square)]
xrayY b sq = xrayWith b sq sightedY

xrayU :: Board -> Square -> [(Square, Square)]
xrayU b sq = xrayWith b sq sightedU

xrayV :: Board -> Square -> [(Square, Square)]
xrayV b sq = xrayWith b sq sightedV

-- | Move
scope :: Board -> Square -> (Colour, Piece) -> Word64
scope b sq (c, P) = (activeP c ! sq .&. hostile b c)
                    .|. (quietP c ! sq .&. unoccupied b .&. activeR b sq)
scope b sq (c, p) = active b sq (c, p) .&. engageable b c

-- | Check
checks :: Board -> Colour -> Word64
checks b c = attacksTo b (locateKing b c) .&. hostile b c

inCheck :: Board -> Colour -> Bool
inCheck b c = checks b c /= zeroBits

-- | Pin

-- | Static
quietP :: Colour -> UArray Square Word64
quietP c = buildWith (\ sq -> translationsXY (ts c sq) sq)
  where
    ts White sq
      | coordY sq == 1 = [(0, 1), (0, 2)]
      | otherwise      = [(0, 1)]
    ts Black sq
      | coordY sq == 6 = [(0, -1), (0, -2)]
      | otherwise      = [(0, -1)]

activeP :: Colour -> UArray Square Word64
activeP c = buildWith (translationsXY (ts c))
  where
    ts White = [(-1, 1), (1, 1)]
    ts Black = [(-1, -1), (1, -1)]

activeN :: UArray Square Word64
activeN = buildWith (translationsXY ts)
  where
    ts = [(i, j) | i <- [-2, -1, 1, 2], j <- [-2, -1, 1, 2] , abs i /= abs j]

activeK :: UArray Square Word64
activeK = buildWith (translationsXY ts)
  where
    ts = [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]

-- | Slide
activeB :: Board -> Square -> Word64
activeB b sq = slideU b sq .|. slideV b sq

activeR :: Board -> Square -> Word64
activeR b sq = slideX b sq .|. slideY b sq

activeQ :: Board -> Square -> Word64
activeQ b sq = activeB b sq .|. activeR b sq

-- | Representations
toPieceList :: Board -> [(Colour, Piece, Square)]
toPieceList b =
  concat $ fmap
    (\ (c, p) -> fmap (\ sq -> (c, p, sq)) $ decodeSquares $ piece b (c, p))
    $ range (minBound, maxBound)

fromPieceList :: [(Colour, Piece, Square)] -> Board
fromPieceList pl = foldl' (\ b (c, p, sq) -> toggle (c, p) sq b) emptyBoard pl

toArray :: Board -> Array Square (Maybe (Colour, Piece))
toArray b = buildSquaresArray (b !?)

fromArray :: Array Square (Maybe (Colour, Piece)) -> Board
fromArray arr =
  fromPieceList
  $ mapMaybe (\ sq -> fmap (\ (c, p) -> (c, p, sq)) (arr ! sq)) enumSquares
