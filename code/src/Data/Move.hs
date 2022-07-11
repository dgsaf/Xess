{- |
Module      : Data.Move
Description :
-}
module Data.Move
  ( Move
  , consCastle
  , consQuiet
  , consPush
  , consPromote
  , consCapture
  , consCaptureEP
  , consCapturePromote
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
consCastle :: (Colour, Side) -> Move
consCastle (c, s) =
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

consQuiet :: Square -> Square -> Move
consQuiet sq sq' =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = Quiet
  , _captured = Nothing
  }

consPush :: Square -> Square -> Move
consPush sq sq' =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = Push
  , _captured = Nothing
  }

consPromote :: Square -> Square -> Piece -> Move
consPromote sq sq' pp =
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

consCapture :: Square -> Square -> (Colour, Piece) -> Move
consCapture sq sq' (c, cp) =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = Capture
  , _captured = Just (c, cp)
  }

consCaptureEP :: Square -> Square -> Colour -> Move
consCaptureEP sq sq' c =
  Move
  { _origin = sq
  , _target = sq'
  , _moveFlag = CaptureEP
  , _captured = Just (c, P)
  }

consCapturePromote :: Square -> Square -> (Colour, Piece) -> Piece -> Move
consCapturePromote sq sq' (c, cp) pp =
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
