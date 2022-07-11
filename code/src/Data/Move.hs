{- |
Module      : Data.Move
Description :
-}
module Data.Move
  ( Move
  , castle
  , quiet
  , push
  , promote
  , capture
  , captureEP
  , capturePromote
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

-- |
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

data Move
  = Move
  { _origin :: Square
  , _target :: Square
  , _moveFlag :: MoveFlag
  , _captured :: Maybe (Colour, Piece)
  }

castle :: (Colour, Side) -> Move
castle (c, s) =
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

quiet :: Square -> Square -> Move
quiet sq sq' =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = Quiet
  , _captured = Nothing
  }

push :: Square -> Square -> Move
push sq sq' =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = Push
  , _captured = Nothing
  }

promote :: Square -> Square -> Piece -> Move
promote sq sq' pp =
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

capture :: Square -> Square -> (Colour, Piece) -> Move
capture sq sq' (c, cp) =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = Capture
  , _captured = Just (c, cp)
  }

captureEP :: Square -> Square -> Colour -> Move
captureEP sq sq' c =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = CaptureEP
  , _captured = Just (c, P)
  }

capturePromote :: Square -> Square -> (Colour, Piece) -> Piece -> Move
capturePromote sq sq' (c, cp) pp =
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
