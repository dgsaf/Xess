{- |
Module      : Data.Move
Description :
-}
module Data.Move
  ( MoveFlag

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
  , _moveFlag :: MoveFlag
  , _captured :: Maybe (Colour, Piece)
  } deriving (Eq, Read, Show)

origin :: Move -> Square
origin = _origin

target :: Move -> Square
target = _target

moveFlag :: Move -> MoveFlag
moveFlag = _moveFlag

captured :: Move -> Maybe (Colour, Piece)
captured = _captured

-- | Constructors
mkCastle :: (Colour, Side) -> Move
mkCastle (c, s) =
  Move
  { _origin = sqK
  , _target = sqK'
  , _moveFlag = flag
  , _captured = Nothing
  }
  where
    (sqK, sqK') = castleSquaresK (c, s)
    flag =
      case (c, s) of
        (White, Short) -> CastleWS
        (White, Long)  -> CastleWL
        (Black, Short) -> CastleBS
        (Black, Long)  -> CastleBL

mkQuiet :: Square -> Square -> Move
mkQuiet sq sq' =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = Quiet
  , _captured = Nothing
  }

mkPush :: Square -> Square -> Move
mkPush sq sq' =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = Push
  , _captured = Nothing
  }

mkPromote :: Square -> Square -> Piece -> Move
mkPromote sq sq' pp =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = flag
  , _captured = Nothing
  }
  where
    flag =
      case pp of
        N -> PromoteN
        B -> PromoteB
        R -> PromoteR
        Q -> PromoteQ
        _ -> undefined

mkCapture :: Square -> Square -> (Colour, Piece) -> Move
mkCapture sq sq' (c, cp) =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = Capture
  , _captured = Just (c, cp)
  }

mkCaptureEP :: Square -> Square -> Colour -> Move
mkCaptureEP sq sq' c =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = CaptureEP
  , _captured = Just (c, P)
  }

mkCapturePromote :: Square -> Square -> (Colour, Piece) -> Piece -> Move
mkCapturePromote sq sq' (c, cp) pp =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = flag
  , _captured = Just (c, cp)
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
mkPromotes :: Square -> Square -> [Move]
mkPromotes sq sq' =
  fmap (mkPromote sq sq') [N, B, R, Q]

mkCapturePromotes :: Square -> Square -> (Colour, Piece) -> [Move]
mkCapturePromotes sq sq' (c, cp) =
  fmap (mkCapturePromote sq sq' (c, cp)) [N, B, R, Q]

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
genQuietP b c = filter pred . fmap f . decodeSquares $ w
  where
    w = stepP b c .&. (complement $ rankPromote c)
    f sq = mkQuiet (behind c sq) sq
    pred mv = hasSquare (mask b (_origin mv)) (_target mv)

genQuietNonP :: Board -> Colour -> [Move]
genQuietNonP b c = concat . fmap f . decodeSquares $ w
  where
    w = colour b c .&. (complement $ piece b (c, P))
    f sq = fmap (mkQuiet sq) . decodeSquares
           $ unoccupied b .&. attacksFrom b sq .&. mask b sq

genPush :: Board -> Colour -> [Move]
genPush b c = filter pred . fmap f . decodeSquares $ w
  where
    w = pushP b c
    f sq = mkPush (behind c . behind c $ sq) sq
    pred mv = hasSquare (mask b (_origin mv)) (_target mv)

genPromote :: Board -> Colour -> [Move]
genPromote b c = filter pred . concat . fmap f . decodeSquares $ w
  where
    w = stepP b c .&. rankPromote c
    f sq = mkPromotes (behind c sq) sq
    pred mv = hasSquare (mask b (_origin mv)) (_target mv)

genCapture :: Board -> Colour -> [Move]
genCapture b c = genCaptureP b c ++ genCaptureNonP b c

genCaptureP :: Board -> Colour -> [Move]
genCaptureP b c = filter pred
  $ (fmap fu . decodeSquares $ wu) ++ (fmap fv . decodeSquares $ wv)
  where
    wu = activePU b c .&. hostile b c .&. (complement $ rankPromote c)
    wv = activePV b c .&. hostile b c .&. (complement $ rankPromote c)
    fu sq = mkCapture (behindU c sq) sq (fromJust $ b !? sq)
    fv sq = mkCapture (behindV c sq) sq (fromJust $ b !? sq)
    pred mv = hasSquare (mask b (_origin mv)) (_target mv)

genCaptureNonP :: Board -> Colour -> [Move]
genCaptureNonP b c = concat . fmap f . decodeSquares $ w
  where
    w = colour b c .&. (complement $ piece b (c, P))
    f sq = fmap (\sq' -> mkCapture sq sq' (fromJust $ b !? sq'))
           . decodeSquares $ hostile b c .&. attacksFrom b sq .&. mask b sq

genCaptureEP :: Board -> Colour -> Maybe Square -> [Move]
genCaptureEP b c Nothing     = []
genCaptureEP b c (Just sqEP) = filter pred . fmap f . decodeSquares $ w
  where
    w = piece b (c, P) .&. (backwardUV c (encodeSquare sqEP))
    f sq = mkCaptureEP sq sqEP c
    pred mv = hasSquare (mask b (_origin mv)) (_target mv)

genCapturePromote :: Board -> Colour -> [Move]
genCapturePromote b c = filter pred
  $ (concat . fmap fu . decodeSquares $ wu)
  ++ (concat . fmap fv . decodeSquares $ wv)
  where
    wu = activePU b c .&. hostile b c .&. rankPromote c
    wv = activePV b c .&. hostile b c .&. rankPromote c
    fu sq = mkCapturePromotes (behindU c sq) sq (fromJust $ b !? sq)
    fv sq = mkCapturePromotes (behindV c sq) sq (fromJust $ b !? sq)
    pred mv = hasSquare (mask b (_origin mv)) (_target mv)
