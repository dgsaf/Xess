{- |
Module      : Data.Board
Description :
-}
module Data.Board
  ( Board
  , emptyBoard, defaultBoard
  , toggle, insertPiece, removePiece, clearSquare

  , piece, colour, occupied
  , hostile, engageable, unoccupied
  , pieceAt, (!?), locateKing
  , areFriendly, areHostile

  , slideX, slideY, slideU, slideV
  , slidingPiecesB, slidingPiecesR
  , slidesX, slidesY, slidesU, slidesV

  , stepP, pushP, activeP, activePU, activePV
  , activeN, activeK
  , activeB, activeR, activeQ

  , active
  , attacksFrom, attacksTo, attacking, attacked

  , isCastleOpen

  , checks, inCheck
  , pinnedX, pinnedY, pinnedU, pinnedV
  , pinnedB, pinnedR, pinned
  , checkMask, kingMask, pinMask, mask

  , toPieceList, fromPieceList
  , toArray, fromArray
  ) where

import Data.Bitboard
import Data.Castle
import Data.Colour
import Data.Pawn
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
    wp = encodeSquaresBy ((==) 1 . coordY)
    wn = encodeSquares $ fmap toEnum [1, 6]
    wb = encodeSquares $ fmap toEnum [2, 5]
    wr = encodeSquares $ fmap toEnum [0, 7]
    wq = encodeSquare $ toEnum 3
    wk = encodeSquare $ toEnum 4
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

-- | Derived Properties
hostile :: Board -> Colour -> Word64
hostile b c = colour b (opposite c)

engageable :: Board -> Colour -> Word64
engageable b c = complement $ colour b c

unoccupied :: Board -> Word64
unoccupied b = complement $ occupied b

-- |
pieceAt :: Board -> Square -> Maybe (Colour, Piece)
pieceAt b sq
  | hasSquare (piece b (White, P)) sq = Just (White, P)
  | hasSquare (piece b (White, N)) sq = Just (White, N)
  | hasSquare (piece b (White, B)) sq = Just (White, B)
  | hasSquare (piece b (White, R)) sq = Just (White, R)
  | hasSquare (piece b (White, Q)) sq = Just (White, Q)
  | hasSquare (piece b (White, K)) sq = Just (White, K)
  | hasSquare (piece b (Black, P)) sq = Just (Black, P)
  | hasSquare (piece b (Black, N)) sq = Just (Black, N)
  | hasSquare (piece b (Black, B)) sq = Just (Black, B)
  | hasSquare (piece b (Black, R)) sq = Just (Black, R)
  | hasSquare (piece b (Black, Q)) sq = Just (Black, Q)
  | hasSquare (piece b (Black, K)) sq = Just (Black, K)
  | otherwise                         = Nothing

(!?) :: Board -> Square -> Maybe (Colour, Piece)
(!?) b sq = pieceAt b sq

locateKing :: Board -> Colour -> Square
locateKing b c
  | n /= 64   = toEnum n
  | otherwise = undefined
  where
    n = countTrailingZeros $ piece b (c, K)

areFriendly :: Board -> Square -> Square -> Bool
areFriendly b sq sq' = fromMaybe False $
  do
    (c, p) <- b !? sq
    (c', p') <- b !? sq'
    return (c == c')

areHostile :: Board -> Square -> Square -> Bool
areHostile b sq sq' = fromMaybe False $
  do
    (c, p) <- b !? sq
    (c', p') <- b !? sq'
    return (c /= c')

-- | Sliding
slideX :: Board -> Square -> Word64
slideX b sq = visibleX (_rotOccupied b) sq `clearBit` fromEnum sq

slideY :: Board -> Square -> Word64
slideY b sq = visibleY (_rotOccupied b) sq `clearBit` fromEnum sq

slideU :: Board -> Square -> Word64
slideU b sq = visibleU (_rotOccupied b) sq `clearBit` fromEnum sq

slideV :: Board -> Square -> Word64
slideV b sq = visibleV (_rotOccupied b) sq `clearBit` fromEnum sq

slidingPiecesB :: Board -> Colour -> Word64
slidingPiecesB b c = piece b (c, B) .|. piece b (c, Q)

slidingPiecesR :: Board -> Colour -> Word64
slidingPiecesR b c = piece b (c, R) .|. piece b (c, Q)

slidesX :: Board -> Colour -> Word64
slidesX b c = bitUnion $ fmap (slideX b) . decodeSquares $ slidingPiecesR b c

slidesY :: Board -> Colour -> Word64
slidesY b c = bitUnion $ fmap (slideY b) . decodeSquares $ slidingPiecesR b c

slidesU :: Board -> Colour -> Word64
slidesU b c = bitUnion $ fmap (slideU b) . decodeSquares $ slidingPiecesB b c

slidesV :: Board -> Colour -> Word64
slidesV b c = bitUnion $ fmap (slideV b) . decodeSquares $ slidingPiecesB b c

-- | Pawn
stepP :: Board -> Colour -> Word64
stepP b c = stepForward (occupied b) c (piece b (c, P))

pushP :: Board -> Colour -> Word64
pushP b c = pushForward (occupied b) c (piece b (c, P))

activeP :: Board -> Colour -> Word64
activeP b c = forwardUV c (piece b (c, P))

activePU :: Board -> Colour -> Word64
activePU b c = forwardU c (piece b (c, P))

activePV :: Board -> Colour -> Word64
activePV b c = forwardV c (piece b (c, P))

-- | Static
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

-- | Attack
active :: Board -> Square -> (Colour, Piece) -> Word64
active b sq (c, P) = forwardUV c (encodeSquare sq)
active b sq (c, N) = activeN ! sq
active b sq (c, B) = activeB b sq
active b sq (c, R) = activeR b sq
active b sq (c, Q) = activeQ b sq
active b sq (c, K) = activeK ! sq

attacksFrom :: Board -> Square -> Word64
attacksFrom b sq = maybe zeroBits (active b sq) (b !? sq)

attacksTo :: Board -> Square -> Word64
attacksTo b sq = bitUnion [aP, aN, aB, aR, aQ, aK]
  where
    f p = (piece b (White, p)) .|. (piece b (Black, p))
    aN = (activeN ! sq) .&. f N
    aB = (activeB b sq) .&. f B
    aR = (activeR b sq) .&. f R
    aQ = (activeQ b sq) .&. f Q
    aK = (activeK ! sq) .&. f K
    g c = forwardUV c (encodeSquare sq) .&. piece b (opposite c, P)
    aP = (g White .|. g Black)

attacking :: Board -> Colour -> Word64
attacking b c = bitUnion $ fmap (attacksFrom b) $ decodeSquares $ colour b c

attacked :: Board -> Colour -> Word64
attacked b c = attacking b $ opposite c

-- | Castling
isCastleOpen :: Board -> (Colour, Side) -> Bool
isCastleOpen b (c, s) = hasK && hasR && isLineOpen && isLineSafe
  where
    (sqK, sqK') = castleSquaresK (c, s)
    (sqR, sqR') = castleSquaresR (c, s)
    hasK = hasSquare (piece b (c, K)) sqK
    hasR = hasSquare (piece b (c, R)) sqR
    isLineOpen = hasSquare (slideY b sqR) sqK
    isLineSafe = (attacked b c .&. lineBetween sqK sqK') == zeroBits

-- | Check
checks :: Board -> Colour -> Word64
checks b c = attacksTo b (locateKing b c) .&. hostile b c

inCheck :: Board -> Colour -> Bool
inCheck b c = checks b c /= zeroBits

-- | Pin
pinnedX :: Board -> Colour -> Word64
pinnedX b c = slideX b (locateKing b c) .&. slidesX b (opposite c)

pinnedY :: Board -> Colour -> Word64
pinnedY b c = slideY b (locateKing b c) .&. slidesY b (opposite c)

pinnedU :: Board -> Colour -> Word64
pinnedU b c = slideU b (locateKing b c) .&. slidesU b (opposite c)

pinnedV :: Board -> Colour -> Word64
pinnedV b c = slideV b (locateKing b c) .&. slidesV b (opposite c)

pinnedB :: Board -> Colour -> Word64
pinnedB b c = pinnedU b c .|. pinnedV b c

pinnedR :: Board -> Colour -> Word64
pinnedR b c = pinnedX b c .|. pinnedY b c

pinned :: Board -> Colour -> Word64
pinned b c = pinnedB b c .|. pinnedR b c

-- | Masks
checkMask :: Board -> Colour -> Word64
checkMask b c =
  bitIntersect . fmap (\ sq -> f sq $ snd . fromJust $ b !? sq) . decodeSquares
  $ checks b c
  where
    sqK = locateKing b c
    f sq N = square ! sq
    f sq _ = lineBetween sqK sq `clearBit` fromEnum sqK

kingMask :: Board -> Colour -> Word64
kingMask b c = complement $ attacked (clearSquare (locateKing b c) b) c

pinMask :: Board -> Square -> Word64
pinMask b sq = maybe (complement zeroBits) f (b !? sq)
  where
    f (c, p)
      | hasSquare (pinnedX b c) sq = slideX b sq
      | hasSquare (pinnedY b c) sq = slideY b sq
      | hasSquare (pinnedU b c) sq = slideU b sq
      | hasSquare (pinnedV b c) sq = slideV b sq
      | otherwise                  = complement zeroBits

mask :: Board -> Square -> Word64
mask b sq = maybe zeroBits f (b !? sq)
  where
    f (c, K) = kingMask b c
    f (c, p) = checkMask b c .&. pinMask b sq

-- | Moves
-- scope :: Board -> Square -> (Colour, Piece) -> Word64
-- scope b sq (c, P) = (activeP c ! sq .&. hostile b c)
--                     .|. (quietP c ! sq .&. unoccupied b .&. slideX b sq)
-- scope b sq (c, p) = active b sq (c, p) .&. engageable b c

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
