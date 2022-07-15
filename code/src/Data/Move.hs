{- |
Module      : Data.Move
Description :
-}
module Data.Move
  ( MoveFlag
    ( CastleWS
    , CastleWL
    , CastleBS
    , CastleBL
    , Quiet
    , Push
    , PromoteN
    , PromoteB
    , PromoteR
    , PromoteQ
    , Capture
    , CaptureEP
    , CapturePromoteN
    , CapturePromoteB
    , CapturePromoteR
    , CapturePromoteQ
    )

  , Move
  , origin, target, moveFlag, captured

  , mkCastle
  , mkQuiet
  , mkPush
  , mkPromote
  , mkCapture
  , mkCaptureEP
  , mkCapturePromote

  , mkPromotes
  , mkCapturePromotes

  , genCastle
  , genQuiet
  , genPush
  , genPromote
  , genCapture
  , genCaptureEP
  , genCapturePromote
  ) where

import Data.Castle
import Data.Bitboard
import Data.Board
import Data.Colour
import Data.Pawn
import Data.Piece
import Data.Square

import Data.Array.IArray
import Data.Bits
import Data.Ix
import Data.Maybe
import Data.Word

-- | Move Flag
data MoveFlag
  = CastleWS
  | CastleWL
  | CastleBS
  | CastleBL
  | Quiet
  | Push
  | PromoteN
  | PromoteB
  | PromoteR
  | PromoteQ
  | Capture
  | CaptureEP
  | CapturePromoteN
  | CapturePromoteB
  | CapturePromoteR
  | CapturePromoteQ
  deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

-- | Move
data Move
  = Move
  { _origin :: Square
  , _target :: Square
  , _moved :: (Colour, Piece)
  , _captured :: Maybe (Colour, Piece)
  , _moveFlag :: MoveFlag
  } deriving (Eq, Read, Show)

origin :: Move -> Square
origin = _origin

target :: Move -> Square
target = _target

moved :: Move -> (Colour, Piece)
moved = _moved

captured :: Move -> Maybe (Colour, Piece)
captured = _captured

moveFlag :: Move -> MoveFlag
moveFlag = _moveFlag

-- | Constructors
mkCastle :: (Colour, Side) -> Move
mkCastle (c, s) =
  Move
  { _origin = sqK
  , _target = sqK'
  , _moved = (c, K)
  , _captured = Nothing
  , _moveFlag = flag
  }
  where
    (sqK, sqK') = castleSquaresK (c, s)
    flag =
      case (c, s) of
        (White, Short) -> CastleWS
        (White, Long)  -> CastleWL
        (Black, Short) -> CastleBS
        (Black, Long)  -> CastleBL

mkQuiet :: Square -> Square -> (Colour, Piece) -> Move
mkQuiet sq sq' (c, p) =
  Move
  { _origin = sq
  , _target = sq'
  , _moved = (c, p)
  , _captured = Nothing
  , _moveFlag = Quiet
  }

mkPush :: Square -> Square -> Colour -> Move
mkPush sq sq' c =
  Move
  { _origin = sq
  , _target = sq'
  , _moved = (c, P)
  , _captured = Nothing
  , _moveFlag = Push
  }

mkPromote :: Square -> Square -> Colour -> Piece -> Move
mkPromote sq sq' c pp =
  Move
  { _origin = sq
  , _target = sq'
  , _moved = (c, P)
  , _captured = Nothing
  , _moveFlag = flag
  }
  where
    flag =
      case pp of
        N -> PromoteN
        B -> PromoteB
        R -> PromoteR
        Q -> PromoteQ
        _ -> undefined

mkCapture :: Square -> Square -> (Colour, Piece) -> Piece -> Move
mkCapture sq sq' (c, p) cp =
  Move
  { _origin = sq
  , _target = sq'
  , _moved = (c, p)
  , _captured = Just (opposite c, cp)
  , _moveFlag = Capture
  }

mkCaptureEP :: Square -> Square -> Colour -> Move
mkCaptureEP sq sq' c =
  Move
  { _origin = sq
  , _target = sq'
  , _moved = (c, P)
  , _captured = Just (opposite c, P)
  , _moveFlag = CaptureEP
  }

mkCapturePromote :: Square -> Square -> Colour -> Piece -> Piece -> Move
mkCapturePromote sq sq' c cp pp =
  Move
  { _origin = sq
  , _target = sq'
  , _moved = (c, P)
  , _captured = Just (opposite c, cp)
  , _moveFlag = flag
  }
  where
    flag =
      case pp of
        N -> CapturePromoteN
        B -> CapturePromoteB
        R -> CapturePromoteR
        Q -> CapturePromoteQ
        _ -> undefined

-- |
mkPromotes :: Square -> Square -> Colour -> [Move]
mkPromotes sq sq' c =
  fmap (mkPromote sq sq' c) [N, B, R, Q]

mkCapturePromotes :: Square -> Square -> Colour -> Piece -> [Move]
mkCapturePromotes sq sq' c cp =
  fmap (mkCapturePromote sq sq' c cp) [N, B, R, Q]

-- |
genCastle :: Board -> Colour -> Castling -> [Move]
genCastle b c ct =
  case (pred Short, pred Long) of
    (True, True)   -> mvS : mvL : []
    (True, False)  -> mvS : []
    (False, True)  -> mvL : []
    (False, False) -> []
  where
    pred s = canCastle ct (c, s) && isCastleOpen b (c, s)
    mvS = mkCastle (c, Short)
    mvL = mkCastle (c, Long)

-- Might be a better way of building pawn moves / using masks
genQuiet :: Board -> Colour -> [Move]
genQuiet b c = genQuietP b c ++ genQuietNonP b c

genQuietP :: Board -> Colour -> [Move]
genQuietP b c = filter (ensureMask b) . fmap f . decodeSquares $ w
  where
    w = stepP b c .&. (complement $ rankPromote c)
    f sq = mkQuiet (behind c sq) sq (c, P)

genQuietNonP :: Board -> Colour -> [Move]
genQuietNonP b c = concat . fmap f . decodeSquares $ w
  where
    w = colour b c .&. (complement $ piece b (c, P))
    f sq = fmap g . decodeSquares $ w'
      where
        w' = attacksFrom b sq .&. unoccupied b .&. mask b sq
        g sq' = mkQuiet sq sq' (fromJust $ pieceAt b sq)

genPush :: Board -> Colour -> [Move]
genPush b c = filter (ensureMask b) . fmap f . decodeSquares $ w
  where
    w = pushP b c
    f sq = mkPush (behind c . behind c $ sq) sq c

genPromote :: Board -> Colour -> [Move]
genPromote b c = filter (ensureMask b) . concat . fmap f . decodeSquares $ w
  where
    w = stepP b c .&. rankPromote c
    f sq = mkPromotes (behind c sq) sq c

genCapture :: Board -> Colour -> [Move]
genCapture b c = genCaptureP b c ++ genCaptureNonP b c

genCaptureP :: Board -> Colour -> [Move]
genCaptureP b c = filter (ensureMask b)
  $ (fmap fu . decodeSquares $ wu) ++ (fmap fv . decodeSquares $ wv)
  where
    wu = activePU b c .&. hostile b c .&. (complement $ rankPromote c)
    wv = activePV b c .&. hostile b c .&. (complement $ rankPromote c)
    fu sq' = mkCapture sq sq' (c, P) p'
      where
        sq = behindU c sq'
        p' = snd . fromJust $ b !? sq'
    fv sq' = mkCapture sq sq' (c, P) p'
      where
        sq = behindV c sq'
        p' = snd . fromJust $ b !? sq'

genCaptureNonP :: Board -> Colour -> [Move]
genCaptureNonP b c = concat . fmap f . decodeSquares $ w
  where
    w = colour b c .&. (complement $ piece b (c, P))
    f sq = fmap g . decodeSquares $ w'
      where
        w' = attacksFrom b sq .&. hostile b c .&. mask b sq
        p = snd . fromJust $ b !? sq
        g sq' = mkCapture sq sq' (c, p) (snd . fromJust $ b !? sq')

genCaptureEP :: Board -> Colour -> Maybe Square -> [Move]
genCaptureEP b c Nothing     = []
genCaptureEP b c (Just sqEP) =
  filter (ensureMask b) . fmap f . decodeSquares $ w
  where
    w = piece b (c, P) .&. (backwardUV c (encodeSquare sqEP))
    f sq = mkCaptureEP sq sqEP c

genCapturePromote :: Board -> Colour -> [Move]
genCapturePromote b c = filter (ensureMask b)
  $ (concat . fmap fu . decodeSquares $ wu)
  ++ (concat . fmap fv . decodeSquares $ wv)
  where
    wu = activePU b c .&. hostile b c .&. rankPromote c
    wv = activePV b c .&. hostile b c .&. rankPromote c
    fu sq' = mkCapturePromotes sq sq' c p'
      where
        sq = behindU c sq'
        p' = snd . fromJust $ b !? sq'
    fv sq' = mkCapturePromotes sq sq' c p'
      where
        sq = behindV c sq'
        p' = snd . fromJust $ b !? sq'

-- |
isMovedPresent :: Board -> Move -> Bool
isMovedPresent b mv = hasSquare (piece b (moved mv)) (origin mv)

isCapturedPresent :: Board -> Move -> Bool
isCapturedPresent b mv = maybe True pred (captured mv)
  where
    pred (c', p') = hasSquare (piece b (c', p')) (target mv)

isCapturedOpposite :: Board -> Move -> Bool
isCapturedOpposite b mv = maybe True pred (captured mv)
  where
    pred (c', p') = c' == opposite (fst $ moved mv)

-- |
ensureMask :: Board -> Move -> Bool
ensureMask b mv = hasSquare (mask b (origin mv)) (target mv)
