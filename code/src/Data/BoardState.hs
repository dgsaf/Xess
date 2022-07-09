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

import Control.Monad
import Data.Bits
import Data.Maybe

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
