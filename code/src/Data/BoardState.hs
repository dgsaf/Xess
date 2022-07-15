{- |
Module      : Data.BoardState
Description :
-}
module Data.BoardState
  ( BoardState
  , board
  , colourToMove
  , castling
  , squareEP
  , halfmove
  , fullmove

  , emptyBoardState
  , defaultBoardState

  , applyMove

  , genMoves
  , genMovesCastle
  , genMovesQuiet
  , genMovesPush
  , genMovesPromote
  , genMovesCapture
  , genMovesCaptureEP
  , genMovesCapturePromote
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

import Data.Maybe

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
applyMove :: BoardState -> Move -> BoardState
applyMove bs mv =
  bs
  { _board = applyMoveBoard mv $ board bs
  , _colourToMove = opposite $ colourToMove bs
  , _castling = applyMoveCastling mv $ castling bs
  , _squareEP = applyMoveSquareEP mv $ squareEP bs
  , _halfmove = applyMoveHalfmove mv $ halfmove bs
  , _fullmove = applyMoveFullmove mv $ fullmove bs
  }

-- Replace insert/remove with toggle when confident working properly
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

applyMoveCastling :: Move -> Castling -> Castling
applyMoveCastling mv =
  maybe id (f (target mv)) (captured mv) . f (origin mv) (moved mv)
  where
    f sq (c, K) = movedK c
    f sq (c, R)
      | sq == initialSquareR (c, Short) = movedR (c, Short)
      | sq == initialSquareR (c, Long)  = movedR (c, Long)
      | otherwise                       = id
    f sq (c, _) = id

applyMoveSquareEP :: Move -> Maybe Square -> Maybe Square
applyMoveSquareEP mv =
  case moveFlag mv of
    Push -> \ _ -> Just (ahead c sq)
    _    -> \ _ -> Nothing
  where
    c = fst . moved $ mv
    sq  = origin mv

applyMoveHalfmove :: Move -> Int -> Int
applyMoveHalfmove mv
  | isIrreversible mv = \ _  -> 0
  | otherwise         = ((+) 1)

applyMoveFullmove :: Move -> Int -> Int
applyMoveFullmove mv
  | fst (moved mv) == White = id
  | otherwise               = ((+) 1)

-- |
genMoves :: BoardState -> [Move]
genMoves bs =
  concat $ fmap (\ f -> f bs)
  [genMovesCastle
  , genMovesQuiet, genMovesPush, genMovesPromote
  , genMovesCapture, genMovesCaptureEP, genMovesCapturePromote]

genMovesCastle :: BoardState -> [Move]
genMovesCastle bs = genCastle (board bs) (colourToMove bs) (castling bs)

genMovesQuiet :: BoardState -> [Move]
genMovesQuiet bs = genQuiet (board bs) (colourToMove bs)

genMovesPush :: BoardState -> [Move]
genMovesPush bs = genPush (board bs) (colourToMove bs)

genMovesPromote :: BoardState -> [Move]
genMovesPromote bs = genPromote (board bs) (colourToMove bs)

genMovesCapture :: BoardState -> [Move]
genMovesCapture bs = genCapture (board bs) (colourToMove bs)

genMovesCaptureEP :: BoardState -> [Move]
genMovesCaptureEP bs = genCaptureEP (board bs) (colourToMove bs) (squareEP bs)

genMovesCapturePromote :: BoardState -> [Move]
genMovesCapturePromote bs = genCapturePromote (board bs) (colourToMove bs)
