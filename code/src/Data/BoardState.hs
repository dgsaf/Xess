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
import Data.Move
import Data.Pawn
import Data.Piece
import Data.Rotated
import Data.Square

-- import Data.Bits
import Data.Maybe
-- import Data.Word

-- | Board State
data BoardState
  = BoardState
  { _board :: Board
  , _colourToMove :: Colour
  , _castling :: Castling
  , _squareEP :: Maybe Square
  , _halfmove :: Int
  , _fullmove :: Int
  } deriving (Eq, Read, Show)

board :: BoardState -> Board
board = _board

colourToMove :: BoardState -> Colour
colourToMove = _colourToMove

castling :: BoardState -> Castling
castling = _castling

squareEP :: BoardState -> Maybe Square
squareEP = _squareEP

halfmove :: BoardState -> Int
halfmove = _halfmove

fullmove :: BoardState -> Int
fullmove = _fullmove

-- | Constructors
emptyBoardState :: BoardState
emptyBoardState =
  BoardState
  { _board = emptyBoard
  , _colourToMove = White
  , _castling = defaultCastling
  , _squareEP = Nothing
  , _halfmove = 0
  , _fullmove = 1
  }

defaultBoardState :: BoardState
defaultBoardState =
  BoardState
  { _board = defaultBoard
  , _colourToMove = White
  , _castling = defaultCastling
  , _squareEP = Nothing
  , _halfmove = 0
  , _fullmove = 1
  }

-- |
-- applyMove :: BoardState -> Move -> BoardState
-- applyMove bs mv =
--   case moveFlag mv of
--     CastleWS        ->
--     CastleWL        ->
--     CastleBS        ->
--     CastleBL        ->
--     Quiet           ->
--     Push            ->
--     PromoteN        ->
--     PromoteB        ->
--     PromoteR        ->
--     PromoteQ        ->
--     Capture         ->
--     CaptureEP       ->
--     CapturePromoteN ->
--     CapturePromoteB ->
--     CapturePromoteR ->
--     CapturePromoteQ ->
--   where
--     b = board bs
--     c  = colourToMove bs
--     c' = opposite c
--     sq  = origin mv
--     sq' = target mv
--     mp  = fmap snd $ b !? sq
--     mp' = fmap snd $ b !? sq'

-- | Replace insert/remove with toggle when confident working properly
applyMoveBoard :: Move -> Board -> Board
applyMoveBoard mv =
  case moveFlag mv of
    CastleWS        ->
      applyMoveBoardCastle (White, Short)
    CastleWL        ->
      applyMoveBoardCastle (White, Long)
    CastleBS        ->
      applyMoveBoardCastle (Black, Short)
    CastleBL        ->
      applyMoveBoardCastle (Black, Long)
    Quiet           ->
      insertPiece (c, p) sq' . removePiece (c, p) sq
    Push            ->
      insertPiece (c, p) sq' . removePiece (c, p) sq
    PromoteN        ->
      insertPiece (c, N) sq' . removePiece (c, P) sq
    PromoteB        ->
      insertPiece (c, B) sq' . removePiece (c, P) sq
    PromoteR        ->
      insertPiece (c, R) sq' . removePiece (c, P) sq
    PromoteQ        ->
      insertPiece (c, Q) sq' . removePiece (c, P) sq
    Capture         ->
      insertPiece (c, p) sq' . removePiece (c, p) sq
      . removePiece (c', fromJust mp') sq'
    CaptureEP       ->
      insertPiece (c, P) sq' . removePiece (c, P) sq
      . removePiece (c', P) (ahead c' sq')
    CapturePromoteN ->
      insertPiece (c, N) sq' . removePiece (c, P) sq
      . removePiece (c', fromJust mp') sq'
    CapturePromoteB ->
      insertPiece (c, B) sq' . removePiece (c, P) sq
      . removePiece (c', fromJust mp') sq'
    CapturePromoteR ->
      insertPiece (c, R) sq' . removePiece (c, P) sq
      . removePiece (c', fromJust mp') sq'
    CapturePromoteQ ->
      insertPiece (c, Q) sq' . removePiece (c, P) sq
      . removePiece (c', fromJust mp') sq'
  where
    (c, p) = moved mv
    sq  = origin mv
    c'  = opposite c
    sq' = target mv
    mp' = fmap snd $ captured mv
    applyMoveBoardCastle (c, s) =
      insertPiece (c, R) sqR' . removePiece (c, R) sqR
      . insertPiece (c, K) sqK' . removePiece (c, K) sqK
      where
        (sqK, sqK') = castleSquaresK (c, s)
        (sqR, sqR') = castleSquaresR (c, s)

applyMoveCastling :: (Colour, Piece) -> Move -> Castling -> Castling
applyMoveCastling (c, p) mv ct =
  case (hasNoCastle ct c, hasNoCastle ct c') of
    (True,  True)  -> ct
    (True,  False) ->
    (False, True)  ->
    (False, False) ->
  where
    c'  = opposite c
    sq  = origin mv
    sq' = target mv
    mp' = fmap snd $ captured mv

    pred = \ c -> canCastle (c, Short) || canCastle (c, Long)

    movK = p == K
    movR s = (p == R) && (sq == initialSquareR (c, s))
    capR s = (maybe False ((==) R) mp') && (sq' == initialSquareR (c', s))

    f

  case moveFlag mv of
    CastleWS        -> movedK c
    CastleWL        -> movedK c
    CastleBS        -> movedK c
    CastleBL        -> movedK c
    Quiet           ->
    Push            ->
    PromoteN        ->
    PromoteB        ->
    PromoteR        ->
    PromoteQ        ->
    Capture         ->
    CaptureEP       ->
    CapturePromoteN ->
    CapturePromoteB ->
    CapturePromoteR ->
    CapturePromoteQ ->
