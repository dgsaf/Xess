{- |
Module      : Data.BoardState
Description :
-}
module Data.BoardState
  (
  ) where

import Data.Bitboard
import Data.Board
import Data.Castle
import Data.Colour
import Data.Piece
import Data.Rotated
import Data.Square

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Maybe
import Data.Word

-- | Board State
data BoardState
  = BoardState
  { _board :: Board
  , _castling :: Castling
  , _squareEP :: Maybe Square
  , _halfmove :: Int
  } deriving (Eq, Read, Show)

-- | Constructors

-- | Incremental Update

-- | Properties
fullmove :: BoardState -> Int
fullmove bs = (_halfmove bs + 1) `div` 2

colourToMove :: BoardState -> Colour
colourToMove bs
  | odd $ _halfmove bs = White
  | otherwise          = Black

-- | Move
newtype Move
  = Move (Square, Square, Maybe Piece)
  deriving (Eq, Read, Show)

src :: Move -> Square
src (Move (sq, sq', mp)) = sq

dst :: Move -> Square
dst (Move (sq, sq', mp)) = sq'

maybePromotion :: Move -> Maybe Piece
maybePromotion (Move (sq, sq', mp)) = mp

-- |
maybeCaptures :: Board -> Move -> Maybe Piece
maybeCaptures b mv
  | areHostile b (src mv) (dst mv) = fmap snd $ b !? dst mv
  | otherwise                      = Nothing

maybePushes :: Board -> Move -> Maybe Square
maybePushes b mv
  | isPawn && abs (n - n') == 16 = Just $ toEnum nEP
  | otherwise                    = Nothing
  where
    isPawn = maybe False (isP . snd) (b !? src mv)
    n  = fromEnum $ src mv
    n' = fromEnum $ dst mv
    nEP = (n + n') `div` 2

maybeCastles :: Board -> Move -> Maybe (Colour, Side)
maybeCastles b mv = mfilter pred $ maybeCastleSquaresK (src mv, dst mv)
  where
    pred (c, s) = hasSquare (piece b (c, K)) . fst $ castleSquaresK (c, s)

-- | Moves
mask :: Board -> Square -> (Colour, Piece) -> Word64
mask b _  (c, K) = kingmask b c
mask b sq (c, p) = checkmask b c .&. pinmask b c sq

moves :: Board -> Square -> (Colour, Piece) -> Maybe Square -> Word64
moves b sq (c, p) msqEP =
  case (p, msqEP) of
    (P, Just sqEP) -> mask b sq (c, p) .&.
                      (scope b sq (c, P) .|. scopeEP b sq (c, P) sqEP)
    (p, _)         -> mask b sq (c, p) .&. scope b sq (c, p)

castles :: Board -> Colour -> Castling -> Word64
castles b c ct =
  case (canCastle ct (c, Short), canCastle ct (c, Long)) of
    (True, True)   -> scopeCastle b (c, Short) .|. scopeCastle b (c, Long)
    (True, False)  -> scopeCastle b (c, Short)
    (False, True)  -> scopeCastle b (c, Long)
    (False, False) -> zeroBits

legalMoves :: BoardState -> [(Square, Square)]
legalMoves bs = moveList ++ castleList
  where
    b = _board bs
    c = colourToMove bs

    sqps =
      mapMaybe
        (\ sq -> liftA2 (,) (Just sq) (fmap snd $ b !? sq))
        $ decodeSquares $ colour b c

    moveList =
      concat $ fmap
        (\ (sq, p) ->
           zip
             (repeat sq)
             (decodeSquares $ moves b sq (c, p) (_squareEP bs)))
        sqps

    castleList =
      zip
        (repeat $ initialSquareK c)
        (decodeSquares (castles b c (_castling bs)))
