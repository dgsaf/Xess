{- |
Module      : Data.Move
Description :
-}
module Data.Move
  ( Move
  , mkCastle
  , mkQuiet
  , mkPush
  , mkPromote
  , mkCapture
  , mkCaptureEP
  , mkCapturePromote
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

-- | Move
data Move
  = Move
  { _origin :: Square
  , _target :: Square
  , _moveFlag :: MoveFlag
  , _captured :: Maybe (Colour, Piece)
  }

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
